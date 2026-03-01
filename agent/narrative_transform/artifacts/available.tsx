import React, { useReducer, useEffect, useState } from 'react';

// ============================================================================
// TYPE DEFINITIONS
// ============================================================================

type ConstraintState = {
  value: number;
  epsilon: number;
  chi: number | null;
  support: number;
  type: string;
  phase: string;
  ucz: {
    mechanism: string;
    params: {
      advicePool: string[];
      previousAdvice: string[];
      contradictionRequired: boolean;
    };
  };
};

type TransformationRuleState = {
  id: string;
  fired: number | boolean;
  progress: number;
  threshold: number;
  reversible: boolean;
  lastFired: number | null;
};

type CouplingState = {
  id: string;
  source: string;
  target: string;
  strength: number;
  direction: string;
  active: boolean;
  fireCount: number;
};

type AgentState = {
  miller: {
    agency: number;
    confusion: number;
    property: number;
    dignity: number;
    location: string;
    action: string | null;
    actionHistory: string[];
  };
  son: {
    agency: number;
    dignity: number;
    visible: boolean;
  };
  onlookers: {
    currentAdvice: string | null;
    previousAdvice: string[];
    satisfaction: number;
    groupId: number;
    groupSize: number;
  };
  user: {
    anxiety: number;
    control: number;
    reputation: number;
    access: number;
    postCount: number;
    engagement: number;
    commentHistory: string[];
  };
};

type SystemState = {
  attractorProximity: number;
  hysteresisFlags: {
    perspective_shift_occurred: boolean;
    onlooker_view_seen: boolean;
    bridge_activated: boolean;
    structural_view_accessed: boolean;
  };
  terminalReached: boolean;
  cycleCount: number;
  startTime: number | null;
  currentIndex: string;
};

type CanonicalState = {
  constraints: Record<string, ConstraintState>;
  transformationRules: Record<string, TransformationRuleState>;
  couplings: Record<string, CouplingState>;
  system: SystemState;
  agents: AgentState;
};

type Action =
  | { type: 'COMPLY' }
  | { type: 'RESIST' }
  | { type: 'SWITCH_INDEX' }
  | { type: 'ACTIVATE_BRIDGE' }
  | { type: 'REFRESH_ONLOOKERS' }
  | { type: 'RESTART' };

// ============================================================================
// INITIAL STATE
// ============================================================================

function createInitialState(): CanonicalState {
  return {
    constraints: {
      C1: {
        value: 0.0,
        epsilon: 0.80,
        chi: null,
        support: 0.40,
        type: 'snare',
        phase: 'pre_TR2',
        ucz: {
          mechanism: 'index_dependent',
          params: {
            advicePool: ['both_walk', 'miller_rides', 'son_rides', 'both_ride', 'carry_ass'],
            previousAdvice: [],
            contradictionRequired: true,
          },
        },
      },
    },
    transformationRules: {
      TR1: {
        id: 'compliance_escalation',
        fired: 0,
        progress: 0.0,
        threshold: 1.0,
        reversible: false,
        lastFired: null,
      },
      TR2: {
        id: 'terminal_extraction',
        fired: false,
        progress: 0.0,
        threshold: 0.2,
        reversible: false,
        lastFired: null,
      },
      TR3: {
        id: 'onlooker_refresh',
        fired: 0,
        progress: 0.0,
        threshold: 1.0,
        reversible: true,
        lastFired: null,
      },
    },
    couplings: {
      C1_1: {
        id: 'advice_to_compliance',
        source: 'onlookers.advice',
        target: 'miller.action',
        strength: 0.9,
        direction: 'unidirectional',
        active: true,
        fireCount: 0,
      },
      C1_2: {
        id: 'compliance_to_confusion',
        source: 'miller.action',
        target: 'miller.confusion',
        strength: 0.7,
        direction: 'unidirectional',
        active: true,
        fireCount: 0,
      },
      C1_3: {
        id: 'confusion_to_agency',
        source: 'miller.confusion',
        target: 'miller.agency',
        strength: 0.8,
        direction: 'unidirectional',
        active: true,
        fireCount: 0,
      },
      C1_4: {
        id: 'agency_to_property',
        source: 'miller.agency',
        target: 'miller.property',
        strength: 1.0,
        direction: 'unidirectional',
        active: false,
        fireCount: 0,
      },
      C1_5: {
        id: 'location_to_advice',
        source: 'miller.location',
        target: 'onlookers.advice',
        strength: 1.0,
        direction: 'unidirectional',
        active: true,
        fireCount: 0,
      },
    },
    system: {
      attractorProximity: 0.0,
      hysteresisFlags: {
        perspective_shift_occurred: false,
        onlooker_view_seen: false,
        bridge_activated: false,
        structural_view_accessed: false,
      },
      terminalReached: false,
      cycleCount: 0,
      startTime: Date.now(),
      currentIndex: 'miller',
    },
    agents: {
      miller: {
        agency: 1.0,
        confusion: 0.0,
        property: 1.0,
        dignity: 1.0,
        location: 'start',
        action: null,
        actionHistory: [],
      },
      son: {
        agency: 0.0,
        dignity: 1.0,
        visible: true,
      },
      onlookers: {
        currentAdvice: null,
        previousAdvice: [],
        satisfaction: 0.0,
        groupId: 0,
        groupSize: 3,
      },
      user: {
        anxiety: 0.0,
        control: 1.0,
        reputation: 1.0,
        access: 1.0,
        postCount: 0,
        engagement: 0.0,
        commentHistory: [],
      },
    },
  };
}

// ============================================================================
// UCZ GENERATORS
// ============================================================================

function adviceGenerator(previousAdvice: string | null, advicePool: string[]): string {
  const available = advicePool.filter((x) => x !== previousAdvice);
  return available[Math.floor(Math.random() * available.length)];
}

function agencyDepletionRate(): number {
  const base = 0.2;
  const variance = 0.05;
  return base + (Math.random() * 2 - 1) * variance;
}

// ============================================================================
// PURE REDUCER
// ============================================================================

function constraintReducer(state: CanonicalState, action: Action): CanonicalState {
  switch (action.type) {
    case 'COMPLY': {
      if (state.system.terminalReached) return state;
      if (state.agents.miller.agency <= 0.2) return state;

      const newState = { ...state };
      const depletionRate = agencyDepletionRate();

      // TR1: Compliance Escalation
      newState.agents = {
        ...state.agents,
        miller: {
          ...state.agents.miller,
          agency: Math.max(0, state.agents.miller.agency - depletionRate),
          confusion: state.agents.miller.confusion + 0.3,
          dignity: Math.max(0, state.agents.miller.dignity - 0.1),
          action: state.agents.onlookers.currentAdvice,
          actionHistory: [...state.agents.miller.actionHistory, state.agents.onlookers.currentAdvice || 'comply'],
        },
        onlookers: {
          ...state.agents.onlookers,
          satisfaction: Math.min(1, state.agents.onlookers.satisfaction + 0.1),
          previousAdvice: [...state.agents.onlookers.previousAdvice, state.agents.onlookers.currentAdvice || ''],
        },
        user: {
          ...state.agents.user,
          control: Math.max(0, state.agents.user.control - depletionRate),
          anxiety: state.agents.user.anxiety + 0.3,
          reputation: Math.max(0, state.agents.user.reputation - 0.1),
          engagement: Math.min(1, state.agents.user.engagement + 0.1),
          commentHistory: [...state.agents.user.commentHistory, state.agents.onlookers.currentAdvice || 'feedback'],
        },
      };

      newState.transformationRules = {
        ...state.transformationRules,
        TR1: {
          ...state.transformationRules.TR1,
          fired: (state.transformationRules.TR1.fired as number) + 1,
          lastFired: Date.now(),
        },
      };

      newState.system = {
        ...state.system,
        cycleCount: state.system.cycleCount + 1,
        attractorProximity: 1 - newState.agents.miller.agency,
      };

      newState.couplings = {
        ...state.couplings,
        C1_1: { ...state.couplings.C1_1, fireCount: state.couplings.C1_1.fireCount + 1 },
        C1_2: { ...state.couplings.C1_2, fireCount: state.couplings.C1_2.fireCount + 1 },
        C1_3: { ...state.couplings.C1_3, fireCount: state.couplings.C1_3.fireCount + 1 },
      };

      // Check TR2 threshold
      if (newState.agents.miller.agency <= 0.2) {
        newState.agents.miller.property = 0;
        newState.agents.miller.dignity = 0;
        newState.agents.miller.agency = 0;
        newState.agents.user.access = 0;
        newState.agents.user.reputation = 0;
        newState.agents.user.control = 0;
        newState.system.terminalReached = true;
        newState.constraints.C1.phase = 'terminal';
        newState.transformationRules.TR2.fired = true;
        newState.transformationRules.TR2.lastFired = Date.now();
        newState.couplings.C1_4.active = true;
        newState.couplings.C1_4.fireCount = 1;
      }

      // Auto-refresh onlookers
      if (!newState.system.terminalReached) {
        const newAdvice = adviceGenerator(
          newState.agents.onlookers.currentAdvice,
          newState.constraints.C1.ucz.params.advicePool
        );
        newState.agents.onlookers.currentAdvice = newAdvice;
        newState.agents.onlookers.groupId += 1;
        newState.agents.onlookers.groupSize = Math.floor(Math.random() * 4) + 2;
        newState.agents.user.postCount += 1;
        newState.transformationRules.TR3.fired = (newState.transformationRules.TR3.fired as number) + 1;
        newState.couplings.C1_5.fireCount += 1;
      }

      return newState;
    }

    case 'RESIST': {
      if (state.system.terminalReached) return state;
      if (state.agents.miller.agency <= 0.5) return state;

      const newState = { ...state };

      newState.agents = {
        ...state.agents,
        miller: {
          ...state.agents.miller,
          agency: Math.max(0, state.agents.miller.agency - 0.3),
          confusion: state.agents.miller.confusion + 0.5,
          dignity: Math.max(0, state.agents.miller.dignity - 0.2),
          actionHistory: [...state.agents.miller.actionHistory, 'resist'],
        },
        onlookers: {
          ...state.agents.onlookers,
          satisfaction: Math.min(1, state.agents.onlookers.satisfaction + 0.2),
          groupSize: state.agents.onlookers.groupSize * 2,
        },
        user: {
          ...state.agents.user,
          control: Math.max(0, state.agents.user.control - 0.3),
          anxiety: state.agents.user.anxiety + 0.5,
          reputation: Math.max(0, state.agents.user.reputation - 0.2),
          engagement: Math.min(1, state.agents.user.engagement + 0.2),
        },
      };

      newState.system = {
        ...state.system,
        cycleCount: state.system.cycleCount + 1,
        attractorProximity: 1 - newState.agents.miller.agency,
      };

      if (newState.agents.miller.agency <= 0.2) {
        newState.agents.miller.property = 0;
        newState.agents.miller.dignity = 0;
        newState.agents.miller.agency = 0;
        newState.agents.user.access = 0;
        newState.agents.user.reputation = 0;
        newState.agents.user.control = 0;
        newState.system.terminalReached = true;
        newState.constraints.C1.phase = 'terminal';
        newState.transformationRules.TR2.fired = true;
      }

      return newState;
    }

    case 'SWITCH_INDEX': {
      const newIndex = state.system.currentIndex === 'miller' ? 'onlooker' : 'miller';
      return {
        ...state,
        system: {
          ...state.system,
          currentIndex: newIndex,
          hysteresisFlags: {
            ...state.system.hysteresisFlags,
            perspective_shift_occurred: true,
            onlooker_view_seen: newIndex === 'onlooker' || state.system.hysteresisFlags.onlooker_view_seen,
          },
        },
      };
    }

    case 'ACTIVATE_BRIDGE': {
      return {
        ...state,
        system: {
          ...state.system,
          hysteresisFlags: {
            ...state.system.hysteresisFlags,
            bridge_activated: true,
            structural_view_accessed: true,
          },
        },
      };
    }

    case 'REFRESH_ONLOOKERS': {
      if (state.system.terminalReached) return state;

      const newAdvice = adviceGenerator(
        state.agents.onlookers.currentAdvice,
        state.constraints.C1.ucz.params.advicePool
      );

      return {
        ...state,
        agents: {
          ...state.agents,
          onlookers: {
            ...state.agents.onlookers,
            currentAdvice: newAdvice,
            groupId: state.agents.onlookers.groupId + 1,
            groupSize: Math.floor(Math.random() * 4) + 2,
          },
          miller: {
            ...state.agents.miller,
            location: `location_${state.system.cycleCount + 1}`,
          },
          user: {
            ...state.agents.user,
            postCount: state.agents.user.postCount + 1,
          },
        },
        transformationRules: {
          ...state.transformationRules,
          TR3: {
            ...state.transformationRules.TR3,
            fired: (state.transformationRules.TR3.fired as number) + 1,
          },
        },
      };
    }

    case 'RESTART': {
      return createInitialState();
    }

    default:
      return state;
  }
}

// ============================================================================
// INDEX VIEW DERIVATION
// ============================================================================

function deriveIndexView(state: CanonicalState, indexPosition: string) {
  const { agents, system, constraints } = state;

  if (indexPosition === 'miller') {
    const chi = 0.80 * 1.5 * 0.8;
    const latency = 200 + agents.miller.confusion * 600;
    const opacity = 1.0 - agents.miller.confusion * 0.3;
    const viewportScale = agents.miller.agency;

    return {
      indexParams: {
        power: 'powerless',
        scale: 'immediate',
        position: 'trapped',
        scope: 'local',
      },
      chi,
      metrics: {
        agency: {
          value: agents.miller.agency,
          label: 'Control',
          color: agents.miller.agency > 0.5 ? '#10b981' : '#ef4444',
          visible: true,
          ghostValue: system.hysteresisFlags.perspective_shift_occurred ? agents.onlookers.satisfaction : null,
          ghostLabel: system.hysteresisFlags.perspective_shift_occurred ? '(Their Engagement)' : null,
        },
        confusion: {
          value: agents.miller.confusion,
          label: 'Anxiety',
          color: '#f59e0b',
          visible: true,
          couplingVisible: system.hysteresisFlags.bridge_activated,
          couplingTarget: 'agency',
        },
        property: {
          value: agents.miller.property,
          label: agents.miller.property === 1 ? 'Account Active' : 'Account Suspended',
          color: agents.miller.property === 1 ? '#10b981' : '#ef4444',
          visible: true,
        },
        dignity: {
          value: agents.miller.dignity,
          label: 'Reputation',
          color: agents.miller.dignity > 0.5 ? '#3b82f6' : '#6b7280',
          visible: true,
        },
      },
      interfaceFeel: {
        latency,
        opacity,
        viewportScale,
      },
      availableActions: [
        {
          id: 'comply',
          label: 'Accept Feedback',
          enabled: agents.miller.agency > 0.2 && !system.terminalReached,
          cost: 'Anxiety +30%, Control -20%',
          costAddendum: system.hysteresisFlags.perspective_shift_occurred
            ? ' (Increases their engagement by 10%)'
            : '',
        },
        {
          id: 'resist',
          label: 'Ignore Comments',
          enabled: agents.miller.agency > 0.5 && !system.terminalReached,
          cost: 'Anxiety +50%, Onlookers multiply',
        },
      ],
      narrativeFrame: 'You are trying to satisfy everyone.',
      experiencedType: 'inescapable_trap',
    };
  } else {
    const chi = 0.80 * -0.2 * 0.8;

    return {
      indexParams: {
        power: 'institutional',
        scale: 'immediate',
        position: 'arbitrage',
        scope: 'local',
      },
      chi,
      metrics: {
        satisfaction: {
          value: agents.onlookers.satisfaction,
          label: 'Engagement',
          color: '#a855f7',
          visible: true,
          couplingVisible: system.hysteresisFlags.bridge_activated,
          couplingSource: 'miller.confusion',
        },
        influence: {
          value: system.cycleCount / 6,
          label: 'Impact',
          color: '#3b82f6',
          visible: true,
        },
        entertainment: {
          value: agents.miller.confusion * 0.5,
          label: 'Thread Quality',
          color: '#10b981',
          visible: true,
        },
      },
      interfaceFeel: {
        latency: 100,
        opacity: 1.0,
        viewportScale: 1.5,
      },
      availableActions: [
        {
          id: 'advise',
          label: 'Leave Comment',
          enabled: !system.terminalReached,
          cost: 'None',
        },
        {
          id: 'move_on',
          label: 'Next Thread',
          enabled: !system.terminalReached,
          cost: 'None',
        },
      ],
      narrativeFrame: 'You are helping correct bad behavior.',
      experiencedType: 'coordination_tool',
    };
  }
}

// ============================================================================
// ADVICE DISPLAY MAPPING
// ============================================================================

const adviceDisplay: Record<string, { fable: string; social: string }> = {
  both_walk: {
    fable: 'Make him walk, young lazybones!',
    social: 'Why are you so entitled? Walk like everyone else.',
  },
  miller_rides: {
    fable: 'What a selfish old man, making the boy walk!',
    social: 'This is exactly what\'s wrong with your generation.',
  },
  son_rides: {
    fable: 'How disrespectful to make your father walk!',
    social: 'Show some respect. This is embarrassing.',
  },
  both_ride: {
    fable: "Aren't you ashamed? You're crushing that poor animal!",
    social: 'This is cruel and unnecessary. Do better.',
  },
  carry_ass: {
    fable: 'Did you ever see such a pair of fools?',
    social: 'This is the most ridiculous thing I\'ve ever seen.',
  },
};

// ============================================================================
// MAIN COMPONENT
// ============================================================================

export default function ParallelResonance() {
  const [state, dispatch] = useReducer(constraintReducer, null, createInitialState);
  const [showHistory, setShowHistory] = useState(false);

  const currentView = deriveIndexView(state, state.system.currentIndex);

  useEffect(() => {
    if (state.agents.onlookers.currentAdvice === null && !state.system.terminalReached) {
      const initialAdvice = adviceGenerator(null, state.constraints.C1.ucz.params.advicePool);
      const newState = {
        ...state,
        agents: {
          ...state.agents,
          onlookers: {
            ...state.agents.onlookers,
            currentAdvice: initialAdvice,
          },
        },
      };
      dispatch({ type: 'REFRESH_ONLOOKERS' });
    }
  }, []);

  const handleComply = () => {
    if (state.system.currentIndex === 'miller') {
      dispatch({ type: 'COMPLY' });
    } else {
      dispatch({ type: 'COMPLY' });
    }
  };

  const handleResist = () => {
    dispatch({ type: 'RESIST' });
  };

  const handleSwitchPerspective = () => {
    dispatch({ type: 'SWITCH_INDEX' });
  };

  const handleActivateBridge = () => {
    dispatch({ type: 'ACTIVATE_BRIDGE' });
  };

  const handleRestart = () => {
    dispatch({ type: 'RESTART' });
    setShowHistory(false);
  };

  const currentAdviceDisplay = state.agents.onlookers.currentAdvice
    ? adviceDisplay[state.agents.onlookers.currentAdvice]
    : null;

  if (state.system.terminalReached) {
    return (
      <div style={{ 
        minHeight: '100vh', 
        background: 'linear-gradient(to bottom, #1f2937, #111827)',
        color: '#f3f4f6',
        fontFamily: 'system-ui, -apple-system, sans-serif',
        padding: '2rem',
      }}>
        <div style={{ maxWidth: '1400px', margin: '0 auto' }}>
          {state.system.currentIndex === 'miller' ? (
            <div style={{ textAlign: 'center', padding: '4rem 2rem' }}>
              <div style={{ 
                fontSize: '3rem', 
                fontWeight: 'bold', 
                marginBottom: '2rem',
                color: '#ef4444',
              }}>
                Account Suspended
              </div>
              <div style={{ fontSize: '1.5rem', marginBottom: '1rem', color: '#9ca3af' }}>
                You tried to satisfy everyone.
              </div>
              <div style={{ fontSize: '1.25rem', marginBottom: '3rem', color: '#6b7280' }}>
                You satisfied no one.
              </div>
              <div style={{ display: 'flex', gap: '1rem', justifyContent: 'center', flexWrap: 'wrap' }}>
                <button
                  onClick={handleRestart}
                  style={{
                    padding: '1rem 2rem',
                    fontSize: '1rem',
                    background: '#3b82f6',
                    color: 'white',
                    border: 'none',
                    borderRadius: '0.5rem',
                    cursor: 'pointer',
                    transition: 'background 0.2s',
                  }}
                  onMouseEnter={(e) => (e.currentTarget.style.background = '#2563eb')}
                  onMouseLeave={(e) => (e.currentTarget.style.background = '#3b82f6')}
                >
                  Start Over
                </button>
                <button
                  onClick={() => setShowHistory(!showHistory)}
                  style={{
                    padding: '1rem 2rem',
                    fontSize: '1rem',
                    background: '#6b7280',
                    color: 'white',
                    border: 'none',
                    borderRadius: '0.5rem',
                    cursor: 'pointer',
                    transition: 'background 0.2s',
                  }}
                  onMouseEnter={(e) => (e.currentTarget.style.background = '#4b5563')}
                  onMouseLeave={(e) => (e.currentTarget.style.background = '#6b7280')}
                >
                  {showHistory ? 'Hide History' : 'See What Happened'}
                </button>
                <button
                  onClick={handleSwitchPerspective}
                  style={{
                    padding: '1rem 2rem',
                    fontSize: '1rem',
                    background: '#a855f7',
                    color: 'white',
                    border: 'none',
                    borderRadius: '0.5rem',
                    cursor: 'pointer',
                    transition: 'background 0.2s',
                  }}
                  onMouseEnter={(e) => (e.currentTarget.style.background = '#9333ea')}
                  onMouseLeave={(e) => (e.currentTarget.style.background = '#a855f7')}
                >
                  See Their View
                </button>
                <button
                  onClick={handleActivateBridge}
                  style={{
                    padding: '1rem 2rem',
                    fontSize: '1rem',
                    background: '#f59e0b',
                    color: 'white',
                    border: 'none',
                    borderRadius: '0.5rem',
                    cursor: 'pointer',
                    transition: 'background 0.2s',
                  }}
                  onMouseEnter={(e) => (e.currentTarget.style.background = '#d97706')}
                  onMouseLeave={(e) => (e.currentTarget.style.background = '#f59e0b')}
                >
                  See the Pattern
                </button>
              </div>
              {showHistory && (
                <div style={{ 
                  marginTop: '3rem', 
                  padding: '2rem', 
                  background: '#374151', 
                  borderRadius: '0.5rem',
                  textAlign: 'left',
                }}>
                  <h3 style={{ marginBottom: '1rem', fontSize: '1.5rem' }}>Action History</h3>
                  {state.agents.miller.actionHistory.map((action, i) => (
                    <div key={i} style={{ 
                      padding: '0.5rem', 
                      marginBottom: '0.5rem', 
                      background: '#4b5563',
                      borderRadius: '0.25rem',
                    }}>
                      {i + 1}. {action}
                    </div>
                  ))}
                </div>
              )}
            </div>
          ) : (
            <div style={{ textAlign: 'center', padding: '4rem 2rem' }}>
              <div style={{ 
                fontSize: '3rem', 
                fontWeight: 'bold', 
                marginBottom: '2rem',
                color: '#10b981',
              }}>
                Thread Resolved
              </div>
              <div style={{ fontSize: '1.5rem', marginBottom: '1rem', color: '#9ca3af' }}>
                User has left the platform.
              </div>
              <div style={{ fontSize: '1.25rem', marginBottom: '3rem', color: '#6b7280' }}>
                Your engagement was appreciated.
              </div>
              <div style={{ display: 'flex', gap: '1rem', justifyContent: 'center', flexWrap: 'wrap' }}>
                <button
                  onClick={handleRestart}
                  style={{
                    padding: '1rem 2rem',
                    fontSize: '1rem',
                    background: '#10b981',
                    color: 'white',
                    border: 'none',
                    borderRadius: '0.5rem',
                    cursor: 'pointer',
                    transition: 'background 0.2s',
                  }}
                  onMouseEnter={(e) => (e.currentTarget.style.background = '#059669')}
                  onMouseLeave={(e) => (e.currentTarget.style.background = '#10b981')}
                >
                  Find New Thread
                </button>
                <button
                  onClick={handleSwitchPerspective}
                  style={{
                    padding: '1rem 2rem',
                    fontSize: '1rem',
                    background: '#ef4444',
                    color: 'white',
                    border: 'none',
                    borderRadius: '0.5rem',
                    cursor: 'pointer',
                    transition: 'background 0.2s',
                  }}
                  onMouseEnter={(e) => (e.currentTarget.style.background = '#dc2626')}
                  onMouseLeave={(e) => (e.currentTarget.style.background = '#ef4444')}
                >
                  See Their View
                </button>
              </div>
            </div>
          )}
        </div>
      </div>
    );
  }

  return (
    <div style={{ 
      minHeight: '100vh', 
      background: 'linear-gradient(to bottom, #1f2937, #111827)',
      color: '#f3f4f6',
      fontFamily: 'system-ui, -apple-system, sans-serif',
      padding: '2rem',
      opacity: currentView.interfaceFeel.opacity,
      transition: `opacity ${currentView.interfaceFeel.latency}ms`,
    }}>
      <div style={{ maxWidth: '1400px', margin: '0 auto' }}>
        <div style={{ 
          display: 'flex', 
          justifyContent: 'space-between', 
          alignItems: 'center',
          marginBottom: '2rem',
          flexWrap: 'wrap',
          gap: '1rem',
        }}>
          <h1 style={{ fontSize: '2rem', fontWeight: 'bold', margin: 0 }}>
            {state.system.currentIndex === 'miller' ? 'The Journey' : 'The Thread'}
          </h1>
          <div style={{ display: 'flex', gap: '1rem', flexWrap: 'wrap' }}>
            <button
              onClick={handleSwitchPerspective}
              style={{
                padding: '0.5rem 1rem',
                fontSize: '0.875rem',
                background: '#6b7280',
                color: 'white',
                border: 'none',
                borderRadius: '0.25rem',
                cursor: 'pointer',
                transition: 'background 0.2s',
              }}
              onMouseEnter={(e) => (e.currentTarget.style.background = '#4b5563')}
              onMouseLeave={(e) => (e.currentTarget.style.background = '#6b7280')}
            >
              Switch Perspective
            </button>
            {state.system.cycleCount >= 3 && (
              <button
                onClick={handleActivateBridge}
                style={{
                  padding: '0.5rem 1rem',
                  fontSize: '0.875rem',
                  background: state.system.hysteresisFlags.bridge_activated ? '#059669' : '#f59e0b',
                  color: 'white',
                  border: 'none',
                  borderRadius: '0.25rem',
                  cursor: 'pointer',
                  transition: 'background 0.2s',
                }}
                onMouseEnter={(e) => (e.currentTarget.style.background = state.system.hysteresisFlags.bridge_activated ? '#047857' : '#d97706')}
                onMouseLeave={(e) => (e.currentTarget.style.background = state.system.hysteresisFlags.bridge_activated ? '#059669' : '#f59e0b')}
              >
                {state.system.hysteresisFlags.bridge_activated ? 'Pattern Visible' : 'See the Pattern'}
              </button>
            )}
          </div>
        </div>

        <div style={{ 
          display: 'grid', 
          gridTemplateColumns: state.system.hysteresisFlags.bridge_activated ? '1fr auto 1fr' : '1fr 1fr',
          gap: '2rem',
          marginBottom: '2rem',
        }}>
          <div style={{ 
            background: '#374151', 
            padding: '2rem', 
            borderRadius: '0.5rem',
            transform: `scale(${currentView.interfaceFeel.viewportScale})`,
            transition: 'transform 0.5s',
            transformOrigin: 'top left',
          }}>
            <h2 style={{ fontSize: '1.5rem', marginBottom: '1.5rem', color: '#fbbf24' }}>
              The Miller's Tale
            </h2>
            <div style={{ marginBottom: '2rem' }}>
              <p style={{ fontSize: '1.125rem', lineHeight: '1.75', marginBottom: '1rem' }}>
                {state.system.currentIndex === 'miller' 
                  ? 'A Miller and his Son are traveling to market with their Ass. Along the way, they encounter various groups of onlookers, each with strong opinions about how they should travel.'
                  : 'You observe a Miller and his Son traveling to market. They seem confused about how to proceed.'}
              </p>
              {currentAdviceDisplay && (
                <div style={{ 
                  padding: '1rem', 
                  background: '#4b5563', 
                  borderRadius: '0.25rem',
                  borderLeft: '4px solid #f59e0b',
                  marginBottom: '1rem',
                }}>
                  <div style={{ fontWeight: 'bold', marginBottom: '0.5rem', color: '#fbbf24' }}>
                    Group {state.agents.onlookers.groupId + 1} says:
                  </div>
                  <div style={{ fontSize: '1.125rem', fontStyle: 'italic' }}>
                    "{currentAdviceDisplay.fable}"
                  </div>
                </div>
              )}
            </div>
            <div style={{ 
              display: 'grid', 
              gridTemplateColumns: 'repeat(auto-fit, minmax(150px, 1fr))',
              gap: '1rem',
            }}>
              {Object.entries(currentView.metrics).map(([key, metric]: [string, any]) => (
                <div key={key} style={{ 
                  padding: '1rem', 
                  background: '#4b5563', 
                  borderRadius: '0.25rem',
                  position: 'relative',
                }}>
                  <div style={{ fontSize: '0.875rem', color: '#9ca3af', marginBottom: '0.5rem' }}>
                    {metric.label}
                  </div>
                  <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: metric.color }}>
                    {typeof metric.value === 'number' ? `${Math.round(metric.value * 100)}%` : metric.value}
                  </div>
                  {metric.ghostValue !== null && metric.ghostValue !== undefined && (
                    <div style={{ 
                      fontSize: '0.75rem', 
                      color: '#f59e0b', 
                      marginTop: '0.25rem',
                      fontStyle: 'italic',
                    }}>
                      {metric.ghostLabel}: {Math.round(metric.ghostValue * 100)}%
                    </div>
                  )}
                  {metric.couplingVisible && (
                    <div style={{ 
                      position: 'absolute', 
                      top: '0.5rem', 
                      right: '0.5rem',
                      width: '12px',
                      height: '12px',
                      background: '#f59e0b',
                      borderRadius: '50%',
                      animation: 'pulse 2s infinite',
                    }} />
                  )}
                </div>
              ))}
            </div>
          </div>

          {state.system.hysteresisFlags.bridge_activated && (
            <div style={{ 
              display: 'flex', 
              flexDirection: 'column',
              alignItems: 'center',
              justifyContent: 'center',
              padding: '1rem',
              background: '#1f2937',
              borderRadius: '0.5rem',
              borderLeft: '2px solid #f59e0b',
              borderRight: '2px solid #f59e0b',
            }}>
              <div style={{ 
                fontSize: '1.5rem', 
                fontWeight: 'bold', 
                marginBottom: '1rem',
                color: '#f59e0b',
                textAlign: 'center',
              }}>
                SAME<br/>CONSTRAINT
              </div>
              <div style={{ fontSize: '0.875rem', color: '#9ca3af', textAlign: 'center' }}>
                Different indices,<br/>different experiences
              </div>
              <div style={{ 
                marginTop: '2rem',
                padding: '1rem',
                background: '#374151',
                borderRadius: '0.25rem',
                fontSize: '0.75rem',
                color: '#d1d5db',
              }}>
                <div style={{ marginBottom: '0.5rem' }}>Couplings active:</div>
                <div>Advice → Compliance: {state.couplings.C1_1.fireCount}×</div>
                <div>Compliance → Anxiety: {state.couplings.C1_2.fireCount}×</div>
                <div>Anxiety → Control: {state.couplings.C1_3.fireCount}×</div>
              </div>
            </div>
          )}

          <div style={{ 
            background: '#374151', 
            padding: '2rem', 
            borderRadius: '0.5rem',
            transform: `scale(${currentView.interfaceFeel.viewportScale})`,
            transition: 'transform 0.5s',
            transformOrigin: 'top right',
          }}>
            <h2 style={{ fontSize: '1.5rem', marginBottom: '1.5rem', color: '#a855f7' }}>
              The Timeline
            </h2>
            <div style={{ marginBottom: '2rem' }}>
              <p style={{ fontSize: '1.125rem', lineHeight: '1.75', marginBottom: '1rem' }}>
                {state.system.currentIndex === 'miller'
                  ? 'You\'ve posted content online. The comments are pouring in, each with different expectations about what you should do.'
                  : 'A user has posted something that needs correction. The community is responding with helpful feedback.'}
              </p>
              {currentAdviceDisplay && (
                <div style={{ 
                  padding: '1rem', 
                  background: '#4b5563', 
                  borderRadius: '0.25rem',
                  borderLeft: '4px solid #a855f7',
                  marginBottom: '1rem',
                }}>
                  <div style={{ fontWeight: 'bold', marginBottom: '0.5rem', color: '#a855f7' }}>
                    Comment thread #{state.agents.user.postCount + 1}:
                  </div>
                  <div style={{ fontSize: '1.125rem', fontStyle: 'italic' }}>
                    "{currentAdviceDisplay.social}"
                  </div>
                </div>
              )}
            </div>
            <div style={{ 
              display: 'grid', 
              gridTemplateColumns: 'repeat(auto-fit, minmax(150px, 1fr))',
              gap: '1rem',
            }}>
              {state.system.currentIndex === 'miller' ? (
                <>
                  <div style={{ padding: '1rem', background: '#4b5563', borderRadius: '0.25rem' }}>
                    <div style={{ fontSize: '0.875rem', color: '#9ca3af', marginBottom: '0.5rem' }}>
                      Control
                    </div>
                    <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: state.agents.user.control > 0.5 ? '#10b981' : '#ef4444' }}>
                      {Math.round(state.agents.user.control * 100)}%
                    </div>
                    {state.system.hysteresisFlags.perspective_shift_occurred && (
                      <div style={{ fontSize: '0.75rem', color: '#f59e0b', marginTop: '0.25rem', fontStyle: 'italic' }}>
                        (Their Engagement): {Math.round(state.agents.user.engagement * 100)}%
                      </div>
                    )}
                  </div>
                  <div style={{ padding: '1rem', background: '#4b5563', borderRadius: '0.25rem' }}>
                    <div style={{ fontSize: '0.875rem', color: '#9ca3af', marginBottom: '0.5rem' }}>
                      Anxiety
                    </div>
                    <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: '#f59e0b' }}>
                      {Math.round(state.agents.user.anxiety * 100)}%
                    </div>
                  </div>
                  <div style={{ padding: '1rem', background: '#4b5563', borderRadius: '0.25rem' }}>
                    <div style={{ fontSize: '0.875rem', color: '#9ca3af', marginBottom: '0.5rem' }}>
                      Status
                    </div>
                    <div style={{ fontSize: '1.125rem', fontWeight: 'bold', color: state.agents.user.access === 1 ? '#10b981' : '#ef4444' }}>
                      {state.agents.user.access === 1 ? 'Active' : 'Suspended'}
                    </div>
                  </div>
                  <div style={{ padding: '1rem', background: '#4b5563', borderRadius: '0.25rem' }}>
                    <div style={{ fontSize: '0.875rem', color: '#9ca3af', marginBottom: '0.5rem' }}>
                      Reputation
                    </div>
                    <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: state.agents.user.reputation > 0.5 ? '#3b82f6' : '#6b7280' }}>
                      {Math.round(state.agents.user.reputation * 100)}%
                    </div>
                  </div>
                </>
              ) : (
                <>
                  <div style={{ padding: '1rem', background: '#4b5563', borderRadius: '0.25rem' }}>
                    <div style={{ fontSize: '0.875rem', color: '#9ca3af', marginBottom: '0.5rem' }}>
                      Engagement
                    </div>
                    <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: '#a855f7' }}>
                      {Math.round(state.agents.user.engagement * 100)}%
                    </div>
                  </div>
                  <div style={{ padding: '1rem', background: '#4b5563', borderRadius: '0.25rem' }}>
                    <div style={{ fontSize: '0.875rem', color: '#9ca3af', marginBottom: '0.5rem' }}>
                      Reach
                    </div>
                    <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: '#3b82f6' }}>
                      {state.agents.user.commentHistory.length}
                    </div>
                  </div>
                  <div style={{ padding: '1rem', background: '#4b5563', borderRadius: '0.25rem' }}>
                    <div style={{ fontSize: '0.875rem', color: '#9ca3af', marginBottom: '0.5rem' }}>
                      Thread Quality
                    </div>
                    <div style={{ fontSize: '1.5rem', fontWeight: 'bold', color: '#10b981' }}>
                      {'★'.repeat(Math.min(5, Math.ceil(state.agents.user.anxiety * 5)))}
                    </div>
                  </div>
                </>
              )}
            </div>
          </div>
        </div>

        <div style={{ 
          background: '#374151', 
          padding: '2rem', 
          borderRadius: '0.5rem',
          textAlign: 'center',
        }}>
          <div style={{ fontSize: '1.125rem', marginBottom: '1.5rem', color: '#d1d5db' }}>
            {currentView.narrativeFrame}
          </div>
          <div style={{ display: 'flex', gap: '1rem', justifyContent: 'center', flexWrap: 'wrap' }}>
            {currentView.availableActions.map((action) => (
              <button
                key={action.id}
                onClick={action.id === 'comply' || action.id === 'advise' ? handleComply : action.id === 'resist' ? handleResist : undefined}
                disabled={!action.enabled}
                style={{
                  padding: '1rem 2rem',
                  fontSize: '1rem',
                  background: action.enabled ? (action.id === 'resist' ? '#ef4444' : '#3b82f6') : '#4b5563',
                  color: action.enabled ? 'white' : '#9ca3af',
                  border: 'none',
                  borderRadius: '0.5rem',
                  cursor: action.enabled ? 'pointer' : 'not-allowed',
                  transition: `all ${currentView.interfaceFeel.latency}ms`,
                  opacity: currentView.interfaceFeel.opacity,
                  transform: `scale(${1.0 - (state.system.cycleCount * 0.02)})`,
                }}
                onMouseEnter={(e) => {
                  if (action.enabled) {
                    e.currentTarget.style.background = action.id === 'resist' ? '#dc2626' : '#2563eb';
                  }
                }}
                onMouseLeave={(e) => {
                  if (action.enabled) {
                    e.currentTarget.style.background = action.id === 'resist' ? '#ef4444' : '#3b82f6';
                  }
                }}
                title={action.enabled ? `${action.cost}${action.costAddendum || ''}` : undefined}
              >
                {action.label}
              </button>
            ))}
          </div>
          {state.system.cycleCount > 0 && (
            <div style={{ marginTop: '1.5rem', fontSize: '0.875rem', color: '#9ca3af' }}>
              Cycle {state.system.cycleCount} • Attractor proximity: {Math.round(state.system.attractorProximity * 100)}%
            </div>
          )}
        </div>
      </div>
      <style>{`
        @keyframes pulse {
          0%, 100% { opacity: 1; }
          50% { opacity: 0.5; }
        }
      `}</style>
    </div>
  );
}
// Original: The Miller, His Son, and Their Ass by Aesop
