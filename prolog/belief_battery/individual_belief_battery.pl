% ============================================================================
% MODULE: individual_belief_battery (Enhanced v2.0)
% ============================================================================
% Formalizes the complete Individual Practical Battery into DR Stack
% Maps all 40 questions (5 Theory + 35 Practice) to constraint classifications
% Includes cascade analysis, dependency checking, and extraction detection
% ============================================================================

:- module(individual_belief_battery, [
    my_context/1,
    belief_classification/2,
    belief_explanation/2,
    institutional_claim/2,
    type_1_error/1,
    cascade_analysis/2,
    belief_count/1
]).

% --- 1. CORE CONTEXT DEFINITION ---
% Defines the indexical parameters for individual-level analysis
% Adjust these based on your actual power/time/options/scope

my_context(context(
    agent_power(individual_moderate),  % Not powerless, not institutional
    time_horizon(biographical),        % Planning 20-50 year horizon
    exit_options(mobile),              % Can change location/systems if needed
    spatial_scope(national)            % Operating within nation-state context
)).

% --- 2. TIER 0: THEORY (5 Universal Mountains) ---
% Things that are true regardless of WHO/WHEN/WHERE parameters
% These hold sub specie aeternitatis

% T1: Logical Necessity
belief_classification(logical_necessity, mountain).
belief_explanation(logical_necessity, 
    'Laws of logic (non-contradiction, excluded middle) hold universally. Cannot coherently deny logic while using logic.').

% T2: Mathematical Truth
belief_classification(mathematical_truth, mountain).
belief_explanation(mathematical_truth,
    'Mathematical structures exist independent of observers. 2+2=4 regardless of who counts or when.').

% T3: Physical Laws
belief_classification(physical_laws, mountain).
belief_explanation(physical_laws,
    'Physics constrains all possible worlds. Thermodynamics, relativity, quantum mechanics are universal. No belief changes conservation of energy.').

% T4: Consciousness Substrate-Dependence (Provisional)
belief_classification(consciousness_substrate, mountain).
belief_explanation(consciousness_substrate,
    'All evidence: consciousness requires physical substrate (brain). No documented cases of consciousness without matter. Provisional Mountain - could be wrong but no counterexamples.').

% T5: Death Finality (Derived from T4)
belief_classification(death_finality, mountain) :-
    belief_classification(consciousness_substrate, mountain).
belief_explanation(death_finality,
    'If consciousness requires substrate (T4), then death (substrate destruction) ends consciousness. Critical implication: enables urgency, blocks afterlife-based Nooses.').

% --- 3. TIER 1: PRACTICE - INDIVIDUAL (35 Questions) ---
% Pragmatic frames based on individual context with moderate power,
% biographical timeframe, mobile exit options, national scope

%% SECTION 1: AGENCY & CHANGE (P1-P3)

% P1: Meaningful Agency
belief_classification(meaningful_agency, rope).
belief_explanation(meaningful_agency,
    'Accept agency as functional tool despite metaphysical uncertainty. Prevents learned helplessness, enables planning/motivation/responsibility. Even if determinism true theoretically, agency belief serves practical goals.').

% P2: Personal Change
belief_classification(personal_change, rope).
belief_explanation(personal_change,
    'Character/habits are malleable through sustained effort. Empirical evidence: people do change substantially. Requires work (Rope, not free), but possible within biographical timeframe.').

% P3: Identity Stability
belief_classification(identity_stability, rope).
belief_explanation(identity_stability,
    'Identity is constructed continuity, not essential core. Requires active maintenance. Memory connects but personality shifts. Reject "finding yourself" - instead build coherent narrative.').

%% SECTION 2: MEANING & PURPOSE (P4-P5)

% P4: Life Meaning
belief_classification(life_meaning, rope).
belief_explanation(life_meaning,
    'No evidence of cosmic purpose. Meaning is constructed through projects/relationships/values, not discovered. Cosmic meaning claims are often Nooses (see: Gita eternal soul enabling subjugation). Constructed meaning preserves autonomy.').

% P5: Suffering Meaning
belief_classification(suffering_meaning, rope).
belief_explanation(suffering_meaning,
    'Suffering has no inherent meaning. Can be interpreted meaningfully retrospectively (optional tool). But rejecting "suffering is meaningful" as Mountain blocks Nooses that demand suffering "for your own good."').

%% SECTION 3: RELATIONSHIPS & COMMITMENT (P6-P7)

% P6: Relationship Meaning
belief_classification(relationship_meaning, rope).
belief_explanation(relationship_meaning,
    'No inherent meaning in blood or ceremony. Investment creates value. Chosen family is valid. Functional relationships are Ropes. Extractive relationships claimed as sacred are Nooses.').

% P7: Family Obligations
belief_classification(family_obligations, varies).
belief_explanation(family_obligations,
    'Not natural Mountain. Often Rope (functional reciprocity). Sometimes Snare (extraction using "family" claim). Evaluate each relationship: reciprocal = Rope, extractive = Snare. Can exit if harmful.').

%% SECTION 4: WORK & VALUE (P8-P9)

% P8: Work Value
belief_classification(work_value, varies).
belief_explanation(work_value,
    'Work not inherently valuable. Some work is Rope (chosen, meaningful, well-compensated). Some work is Snare (extractive, necessary only for survival). "Work as dignity" claim is Snare preventing questioning of exploitation. Evaluate each job.').

% P9: Wealth/Status Pursuit
belief_classification(wealth_status_pursuit, rope).
belief_explanation(wealth_status_pursuit,
    'Not inherent goods. Are tools for securing autonomy. Pursue sufficient wealth to escape extraction, then diminishing returns. "More is always better" is Snare (keeps you on treadmill). Stop when autonomous.').

%% SECTION 5: EPISTEMIC NORMS (P10-P11)

% P10: Belief Norms (Truth vs Utility)
belief_classification(belief_norms, context_dependent).
belief_explanation(belief_norms,
    'Domain-dependent: High-stakes/power domains → track truth (eternal soul belief enables subjugation). Low-stakes domains → utility acceptable (believing in yourself). Personal relationships → sometimes useful beliefs better. Critical: power analysis requires truth.').

% P11: Moral Intuitions
belief_classification(moral_intuitions, rope).
belief_explanation(moral_intuitions,
    'Intuitions are real but not infallible. Useful heuristics requiring examination. Starting point, not endpoint. Cultural/evolutionary origin does not invalidate. Should examine, not blindly trust or completely ignore.').

%% SECTION 6: POWER & SYSTEMS (P12-P13)

% P12: Power Structures Natural
belief_classification(power_structures_natural, varies).
belief_explanation(power_structures_natural,
    'This IS the meta-question enabling all DR analysis. Most structures are Ropes or Nooses, not Mountains. Some coordination is Rope (traffic rules). Some hierarchy is Snare (claimed as natural). Apply DR framework system-by-system. Key skill: distinguishing them.').

% P13: Participation in Nooses
belief_classification(participation_in_nooses, rope).
belief_explanation(participation_in_nooses,
    'Given moderate power and need for resources: Pure exit often impossible. Pure participation enables extraction. Rope: Strategic participation + resistance where possible. Use system resources to build power to eventually change system.').

%% SECTION 7: DEATH & RISK (P14-P16)

% P14: Mortality Acceptance
belief_classification(mortality_acceptance, mountain) :- 
    belief_classification(death_finality, mountain).
belief_explanation(mortality_acceptance,
    'Forced by T5 (death is final). Not pragmatic choice - this is high-stakes power domain. Afterlife beliefs enable Nooses (die for the cause, sacrifice now for later). Must accept Mountain: death is final. Urgency follows necessarily.').

% P15: Risk Taking
belief_classification(risk_taking, rope).
belief_explanation(risk_taking,
    'Risk tolerance is personal preference within ergodic constraints (Mountain). Never risk ruin even for positive EV. Can choose risk level within survival bounds. Depends on resources, obligations, life stage. Ergodicity is Mountain, preference is Rope.').

% P16: Future Planning
belief_classification(future_planning, rope).
belief_explanation(future_planning,
    'Some planning necessary (coordination with future self). Some present living necessary (life is finite). Balance depends on age, health, resources, discount rate. Neither pure planning nor pure present-maximizing is optimal. Strategic balance.').

%% SECTION 8: TRUST & COOPERATION (P17-P19)

% P17: Trust Default
belief_classification(trust_default, rope).
belief_explanation(trust_default,
    'Trust is coordination mechanism (Rope). Optimal: conditional cooperation (tit-for-tat). Start with trust in low-stakes, verify before high-stakes. Withdraw if exploited. Forgive single defections, punish patterns. Adjust based on environment.').

% P18: Human Nature
belief_classification(human_nature, rope).
belief_explanation(human_nature,
    'People are situational (empirical). Not inherently good (naive trust → exploitation) or bad (authoritarianism). Both extremes are Nooses. People respond to incentives and culture. Design systems assuming mixed motives.').

% P19: Defector Punishment
belief_classification(defector_punishment, rope).
belief_explanation(defector_punishment,
    'Punishment maintains cooperation (Rope) but is costly. Punish if cost < benefit of maintaining cooperation. Or exit relationship. Never always cooperate (enables exploitation). Never always defect (destroys cooperation). Conditional strategy.').

%% SECTION 9: IDENTITY & EXPRESSION (P20-P22)

% P20: Social Conformity
belief_classification(social_conformity, rope).
belief_explanation(social_conformity,
    'Conformity is coordination mechanism. Some norms are functional Ropes (traffic, manners). Some are Nooses (oppressive expectations). Use DR recursively: distinguish Ropes from Nooses, then conform to Ropes, resist Nooses. Build power to resist effectively.').

% P21: Gender/Sexual Identity
belief_classification(gender_sexual_identity, mountain_and_rope).
belief_explanation(gender_sexual_identity,
    'Hybrid: Underlying orientation may be Mountain (discovered). Expression and understanding are Rope (constructed). Strategic: Claim Mountain publicly (protects rights). Allow Rope privately (permits evolution). Both valid.').

% P22: Cultural/Religious Identity
belief_classification(cultural_religious_identity, rope).
belief_explanation(cultural_religious_identity,
    'Not innate. Constructed through participation. Evaluate: Is this identity Rope (community, meaning, low cost) or Snare (extraction, oppression) FOR ME? Can maintain, exit, or construct hybrid. "Must maintain or betray ancestors" is Snare.').

%% SECTION 10: CONSUMPTION & RESOURCES (P23-P25)

% P23: Consumption Level (How Much is Enough)
belief_classification(consumption_level, rope).
belief_explanation(consumption_level,
    'Personal preference subject to hedonic adaptation (Mountain). Secure autonomy threshold (healthcare, housing, food). Beyond that: diminishing returns. Maximize autonomy, not consumption. "More is better" is Snare. Stop when autonomous.').

% P24: Experiences vs Possessions
belief_classification(experiences_vs_possessions, rope).
belief_explanation(experiences_vs_possessions,
    'Personal preference informed by evidence (research shows experiences generally better). Some possessions enable experiences (tools). Some experiences create lasting value (skills, relationships). Neither is Mountain. Avoid: possessions for status, experiences for FOMO.').

% P25: Giving Obligation
belief_classification(giving_obligation, rope).
belief_explanation(giving_obligation,
    'Not absolute obligation. Coordination problem: everyone benefits from safety net. Choose level based on values and resources. Prefer systemic solutions over charity. Effective altruism is one valid Rope, not only option. "Must give everything" can be Snare.').

%% SECTION 11: KNOWLEDGE & LEARNING (P26-P28)

% P26: Specialization
belief_classification(specialization, rope).
belief_explanation(specialization,
    'Strategic choice. Depends on field, life stage, risk tolerance, interest. Some fields reward depth, some breadth. Optimal often T-shaped: deep expertise + broad competence. Can specialize where returns high, generalize where flexibility valuable.').

% P27: Formal Education
belief_classification(formal_education, rope).
belief_explanation(formal_education,
    'Not necessary for all paths. Is Rope for credentialed fields (medicine, law). May be Snare if high debt for low-return field. Field-dependent cost-benefit. Consider alternatives: bootcamps, apprenticeship, self-teaching.').

% P28: Curiosity vs Practicality
belief_classification(curiosity_vs_practicality, rope).
belief_explanation(curiosity_vs_practicality,
    'Balance is strategic. If resources scarce: prioritize practicality (survival). If sufficient: follow curiosity. Optimal: find intersection (paid for curious work). Pure practicality can be Snare if suppresses all interest. Pure curiosity requires independent means.').

%% SECTION 12: POLITICAL PARTICIPATION (P29-P31)

% P29: Electoral Participation
belief_classification(electoral_participation, rope).
belief_explanation(electoral_participation,
    'Not absolute duty. Strategic coordination mechanism. Vote if swing district or down-ballot matters. Don\'t vote if safe district and better uses of energy. Electoral politics is one lever among many. Don\'t mistake voting for only political action.').

% P30: Direct Action/Mutual Aid
belief_classification(direct_action, rope).
belief_explanation(direct_action,
    'Strategic choice. Often more effective than electoral politics. Requires sustained energy. Engage where capacity exists. Focus on mutual aid (builds dual power). Sustainable > heroic. "Must constantly organize" can become Snare (burnout).').

% P31: Local vs Global Issues
belief_classification(local_vs_global, rope).
belief_explanation(local_vs_global,
    'Both matter. Strategic choice based on tractability. Act locally where you can make measurable difference. Support global where leverage exists. Depends on resources, skills, values. Think globally, act locally is cliché but reasonable.').

%% SECTION 13: EXISTENTIAL STANCE (P32-P35)

% P32: Optimism/Pessimism
belief_classification(optimism_pessimism, rope).
belief_explanation(optimism_pessimism,
    'Strategic stance for action, not truth claim (future is uncertain). Pessimism about systems (clear-eyed about Nooses). Optimism about agency (can build alternatives). Gramsci: "Pessimism of intellect, optimism of will." Pure optimism ignores threats. Pure pessimism paralyzes.').

% P33: Cosmic Meaning
belief_classification(cosmic_meaning, rope).
belief_explanation(cosmic_meaning,
    'No evidence of cosmic meaning. Human meaning-making is real and valuable even if constructed. Create meaning through projects/relationships/values. Reject cosmic meaning claims as Nooses (divine duty etc.). Connected to P4 and Gita analysis.').

% P34: Having Children
belief_classification(having_children, rope).
belief_explanation(having_children,
    'Not biological imperative. Massive commitment. Evaluate actual desire (not social pressure), resources, capacity, world conditions. Either choice valid. "Must have children" is Snare (reproductive coercion). "Must not" is also Snare. Genuine choice.').

% P35: Time Allocation
belief_classification(time_allocation, rope).
belief_explanation(time_allocation,
    'Time is finite (Mountain - biological limit). How to spend it is choice (Rope). Given mortality: this is only life, time is gone forever. Choose based on values not shoulds. "Maximize productivity" is Snare (capitalism internalized). Balance: resources, relationships, projects, rest.').

% --- 4. INSTITUTIONAL NOOSE DETECTION ---
% Common claims made by institutions to enable extraction
% These are Type I Errors (False Mountains): Nooses claimed as Mountains

institutional_claim(eternal_soul, mountain).
institutional_claim(cosmic_purpose, mountain).
institutional_claim(natural_hierarchy, mountain).
institutional_claim(work_as_dignity, mountain).
institutional_claim(family_sacred, mountain).
institutional_claim(national_duty, mountain).
institutional_claim(gender_roles_natural, mountain).
institutional_claim(property_rights_natural, mountain).

% Extraction Points (Type I Errors)
type_1_error(eternal_soul).
    % Enables: subjugation ("die for the cause, get reward later")
type_1_error(cosmic_purpose).
    % Enables: control ("this is your divinely ordained role")
type_1_error(natural_hierarchy).
    % Enables: oppression ("some people are naturally superior")
type_1_error(work_as_dignity).
    % Enables: exploitation ("work is virtuous, don't question conditions")
type_1_error(family_sacred).
    % Enables: abuse ("you must tolerate this because family")
type_1_error(national_duty).
    % Enables: war ("die for your country")
type_1_error(gender_roles_natural).
    % Enables: discrimination ("biology determines social role")
type_1_error(property_rights_natural).
    % Enables: concentration ("wealth distribution is natural order")

% --- 5. CASCADE ANALYSIS ---
% What follows if you accept/reject each belief

cascade_analysis(eternal_soul, accept) :-
    format('If eternal soul → duty becomes obligation → can demand your death → subjugation~n').

cascade_analysis(eternal_soul, reject) :-
    format('If mortal soul → urgency in living → cannot be compelled to sacrifice for afterlife~n').

cascade_analysis(meaningful_agency, accept) :-
    format('If agency → planning makes sense → effort is meaningful → responsibility exists~n').

cascade_analysis(meaningful_agency, reject) :-
    format('If no agency → learned helplessness → no point planning → motivation collapse~n').

cascade_analysis(life_meaning, accept_cosmic) :-
    format('If cosmic meaning → must discover purpose → failure = crisis → potential for control~n').

cascade_analysis(life_meaning, accept_constructed) :-
    format('If constructed meaning → freedom to create → responsibility for meaningfulness → autonomy~n').

% --- 6. UTILITY FUNCTIONS ---

% Count beliefs by type
belief_count(Type) :-
    findall(B, belief_classification(B, Type), Beliefs),
    length(Beliefs, Count),
    format('~w beliefs: ~d~n', [Type, Count]).

% List all beliefs of a type
beliefs_of_type(Type, Beliefs) :-
    findall(B, belief_classification(B, Type), Beliefs).

% --- 7. TESTS ---

:- begin_tests(individual_battery_v2).

test(tier0_count) :-
    % Should have exactly 5 Tier 0 Mountains
    findall(B, (belief_classification(B, mountain), \+ member(B, [mortality_acceptance])), T0),
    length(T0, 5).

test(tier1_count) :-
    % Should have 35 Tier 1 classifications
    findall(B, (belief_classification(B, Type), Type \= mountain), T1),
    length(T1, Count),
    Count >= 35.

test(mortality_follows_substrate) :-
    belief_classification(death_finality, mountain),
    belief_classification(mortality_acceptance, mountain).

test(no_cosmic_meaning) :-
    belief_classification(cosmic_meaning, rope),
    \+ belief_classification(cosmic_meaning, mountain).

test(type_1_errors_detected) :-
    findall(E, type_1_error(E), Errors),
    length(Errors, Count),
    Count >= 5.

:- end_tests(individual_battery_v2).
