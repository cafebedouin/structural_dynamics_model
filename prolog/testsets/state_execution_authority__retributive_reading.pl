% ============================================================================
% CONSTRAINT STORY: state_execution_authority__retributive_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_state_execution_authority__retributive_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: state_execution_authority__retributive_reading
 *   human_readable: State Execution Authority (Retributive Reading): Moral Balance Restoration
 *   domain: criminal_justice/political_philosophy/constitutional_law
 *
 * SUMMARY:
 *   This constraint instantiates the RETRIBUTIVE READING of the state
 *   execution authority kernel. The reading claims that execution restores
 *   moral balance by imposing proportionate punishment for heinous crimes.
 *   Under this reading, execution is not merely punishment but a restorative
 *   mechanism: the offender's death settles a moral debt incurred by the
 *   heinous crime. Victims' families are reframed as beneficiaries — they
 *   receive the proportionate punishment their harm demands. The executed
 *   offender is reconstructed from 'rights-bearer to be protected from cruel
 *   punishment' (abolition reading) to 'moral agent who incurred a debt
 *   payable by their death' (retributive reading). The structural consequence
 *   is high extractiveness (0.68): the constraint extracts the offender's
 *   life as proportionate cost and suppresses alternative readings
 *   (deterrence, abolition) by naturalizing execution as the unique
 *   appropriate response to heinous crime. Theater_ratio is low (0.35)
 *   because the retributive reading emphasizes the functional necessity of
 *   execution — it is not performative from the retributive perspective; it
 *   is the literal mechanism of moral restoration. The measurements show
 *   rising extractiveness and suppression over the interval (years 0–20),
 *   reflecting historical shifts: as procedural safeguards (appeals, clemency
 *   review) became institutionalized, the suppression requirement rose to
 *   maintain the constraint's legitimacy.
 *
 * KEY AGENTS:
 *   - Executed Offender: Primary victim (powerless/trapped) — bears maximum extraction (death) with no alternatives; suppressed by legal procedure
 *   - Offender's Kin and Communities: Secondary victims (powerless/trapped) — experience stigma, severed relationships, selective enforcement; trapped by association
 *   - Victims' Families: Beneficiaries (moderate/constrained) — reframed as agents whose need for proportionate punishment justifies the constraint; have constrained exit but genuine coordination function
 *   - Judicial System: Institutional actor (institutional/constrained) — administers proportionate penalties and coordinates victim voice; experiences extraction through dependency on execution for legitimacy
 *   - Retributive Justice Tradition: Institutional beneficiary (institutional/arbitrage) — benefits from execution as proof of proportionality doctrine; can exit via alternative proportionality metrics
 *   - Abolition Movement: Organized opposition (organized/constrained) — experiences constraint as snare coordinated against their core commitment; constrained but organized
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees full structure: reframing of beneficiaries, suppression of alternatives, naturalization of execution as unique proportionate response
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(state_execution_authority__retributive_reading, 0.68).
domain_priors:suppression_score(state_execution_authority__retributive_reading, 0.72).
domain_priors:theater_ratio(state_execution_authority__retributive_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(state_execution_authority__retributive_reading, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(state_execution_authority__retributive_reading, snare).
narrative_ontology:human_readable(state_execution_authority__retributive_reading, "State Execution Authority (Retributive Reading): Moral Balance Restoration").
narrative_ontology:topic_domain(state_execution_authority__retributive_reading, "criminal_justice/political_philosophy/constitutional_law").

domain_priors:requires_active_enforcement(state_execution_authority__retributive_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(state_execution_authority__retributive_reading, 'dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82').
narrative_ontology:cs_kernel_codification('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', formalized).
narrative_ontology:cs_authority_grounding('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', lineage).
narrative_ontology:cs_interpretation_layer_present('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82').
narrative_ontology:cs_reading_relation('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', state_execution_authority__abolition_reading, coexists_with).
narrative_ontology:cs_reading_relation('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', state_execution_authority__deterrence_reading, influences).
narrative_ontology:cs_axiom('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', foundational, execution_proportionate_heinous_crime).
narrative_ontology:cs_axiom_status(execution_proportionate_heinous_crime, holdable).
narrative_ontology:cs_axiom_grounding('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', execution_proportionate_heinous_crime, deontological).
narrative_ontology:cs_axiom('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', foundational, moral_balance_restoration_justifies_death).
narrative_ontology:cs_axiom_status(moral_balance_restoration_justifies_death, holdable).
narrative_ontology:cs_axiom_grounding('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', moral_balance_restoration_justifies_death, deontological).
narrative_ontology:cs_reference_frame('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', classical_retributive_proportionality).
narrative_ontology:cs_drift_state('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', contemporary_human_rights_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('dc0ff8d1-7caa-4c4e-9fb6-a9bd73ef5c82', '').
narrative_ontology:cs_kernel_id(state_execution_authority__retributive_reading, state_execution_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, victims_families).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, state_moral_authority).
narrative_ontology:constraint_beneficiary(state_execution_authority__retributive_reading, retributive_justice_tradition).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, executed_offender).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, offender_kin).
narrative_ontology:constraint_victim(state_execution_authority__retributive_reading, procedural_error_targets).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CONDEMNED OFFENDER (SNARE) — Structurally trapped. No exit: conviction + sentencing + execution are sequentially enforced. The offender bears maximum extraction (death) with no alternatives or negotiation space. The constraint's suppression mechanism is total — legal procedures, appeals, and clemency are performative gates that rarely prevent execution. The offender experiences the constraint as pure extraction with maximal suppression.
constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: OFFENDER'S KIN AND COMMUNITIES (SNARE) — Structurally trapped by association. Families of executed offenders experience permanent stigma, severed relationships, and inherited trauma. Communities experiencing mass executions (disproportionately low-income and racialized groups in historical practice) face extraction through selective enforcement. Suppression is high: social shame, lack of institutional redress, and normalization of the practice prevent resistance. No meaningful exit; extracted through kinship and community membership.
constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: VICTIMS' FAMILIES — RETRIBUTIVE READING (ROPE) — Moderate power. Under the retributive reading, victims' families are reframed as beneficiaries: execution restores moral balance on their behalf. They have constrained exit (cannot negotiate away the execution) but genuine coordination function (their closure/healing is the stated purpose). The constraint coordinates victim voice into state justice authority. From this perspective, the mechanism is pure coordination: the state solves the victims' need for proportionate punishment. Extraction is reframed as legitimate cost redistribution — the offender pays for the harm they caused.
constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: JUDICIAL SYSTEM (TANGLED ROPE) — Institutional actor with constrained exit (bound by constitutional law and public mandate to administer capital punishment where legislated). The system experiences genuine coordination: it administers proportionate penalties according to codified offense severity (coordination function). It also experiences extraction: the system's legitimacy depends on executing heinous offenders; procedural failures are catastrophic for state authority, creating suppression of exculpatory evidence and due process shortcuts. Moderate extractiveness: the system coordinates accountability while extracting authority from the execution mechanism.
constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: RETRIBUTIVE JUSTICE TRADITION (ROPE) — Institutional beneficiary with arbitrage exit. The tradition benefits from execution as proof of proportionate punishment doctrine; it can exit by shifting to alternative proportionality metrics (life imprisonment, restitution). The tradition experiences the constraint as pure coordination: execution embodies the principle that punishment must fit the crime. From this perspective, the constraint solves the coordination problem of calibrating justice to offense severity. No extraction is perceived — the mechanism is seen as vindicating moral principle.
constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: ABOLITION MOVEMENT (SNARE) — Organized agents (death penalty abolitionists, human rights organizations) experience execution as a snare coordinated against their core commitment: that state execution is categorically impermissible. They are structurally constrained (cannot prevent executions through democratic process in retentionist jurisdictions) but organized enough to resist. Extraction runs high: each execution validates the retributive framework and suppresses alternative legitimacy claims. The movement sees the suppression mechanism clearly — the retributive narrative forecloses their core premise (categorical impermissibility) by framing execution as restorative rather than violent.
constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (SNARE) — From civilizational analytical perspective, the constraint's structure is revealed: (1) Victims' families are reframed as beneficiaries to legitimize extraction from the offender and offender's kin; (2) The executed offender is maximally suppressed (no appeal, clemency rarely granted); (3) Procedural errors (wrongful convictions) are treated as tragic mistakes that do not invalidate the framework — the framework's legitimacy does not depend on zero errors, only on the proportionality claim; (4) The constraint extracts authority from the execution mechanism itself, making the state's capacity to execute central to its moral legitimacy; (5) Alternative readings (deterrence, abolition) are suppressed by the retributive axiom that makes execution the uniquely appropriate response to heinous crime. The analytical observer sees this as a snare coordinated by the retributive tradition to preserve execution as state prerogative.
constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(state_execution_authority__retributive_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(state_execution_authority__retributive_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(state_execution_authority__retributive_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(state_execution_authority__retributive_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): The constraint extracts the offender's life as proportionate cost. Under the retributive reading, this is not experienced as extraction by the beneficiaries (victims' families, retributive tradition) but as coordination — the state solving the problem of how to restore moral balance. However, the analytical observer sees the mechanism: the constraint is high-extraction precisely because it redefines victims' families as beneficiaries and redefines the offender's death as a legitimate cost rather than an impermissible harm. The extractiveness value reflects this structural asymmetry. Suppression (0.72): High suppression reflects that alternatives are suppressed by the reading's core axiom. Alternative readings (abolition, deterrence) are foreclosed by the proportionality principle — execution is not merely one option among penalties, but the unique response proportionate to heinous crime. Procedural suppression is also high: appeals and clemency review are performative gates (theater_ratio=0.35) that rarely prevent execution; exculpatory evidence is often suppressed by procedural rules; the offender has minimal meaningful exit. Theater_ratio (0.35): Low. The retributive reading emphasizes that execution is functionally necessary for moral restoration — it is not ceremonial from this perspective. Appeals, clemency review, and proportionality findings are described as serious procedure, not theater. The low theater reflects the reading's internal coherence from the beneficiary perspective. The analytical observer sees higher theater (the rituals of proportionality finding, the deference to victims' families' testimony) but the base_properties reflect the reading's own self-description.
 *
 * PERSPECTIVAL GAP:
 *   The constraint generates maximum perspectival divergence. The executed offender sees pure snare (trapped, zero agency, maximum extraction). Offender's kin see snare (trapped by association, inherited extraction). Victims' families see rope (pure coordination under the retributive reading — their need for proportionate punishment is the constraint's justification). The judicial system sees tangled rope (genuine coordination of proportionate penalties mixed with extraction of authority). The retributive tradition sees rope (pure coordination of the proportionality principle). The abolition movement sees snare (organized opposition to categorically impermissible state violence). The analytical observer sees snare (recognizes the structural mechanism: reframing of beneficiaries, suppression of alternatives, naturalization of execution). The gap exists because the retributive reading's legitimacy claim (moral balance restoration for victims' families) directly contradicts the alternative readings' legitimacy claims (categorical impermissibility for abolition; crime prevention for deterrence). No single classification can hold all perspectives simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) captures each agent's structural relationship to THIS constraint under the retributive reading. Executed offender: d ≈ 0.98 (full target, maximum extraction). Offender's kin: d ≈ 0.92 (near-full target, extracted through association). Victims' families: d ≈ 0.25 (beneficiary in retributive framing, but with constrained exit — they cannot negotiate away the execution). Judicial system: d ≈ 0.60 (mixed — institution benefits from having the constraint but also constrained by it). Retributive tradition: d ≈ 0.10 (beneficiary with arbitrage — benefits from execution but can substitute alternative proportionality metrics). Abolition movement: d ≈ 0.78 (organized opposition, constrained by democratic voting rules). Analytical observer: d ≈ 0.75 (sees the structure clearly, cannot escape the framing within the retributive reading itself). These d values are not overridden — they are derived from power levels, exit options, and beneficiary/victim declarations.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved through reading-specific structuration. The retributive reading does NOT claim to resolve whether execution is good policy or moral justice (preference omega). It claims that IF one accepts the retributive principle (proportionate punishment restores moral balance), THEN execution is the structurally necessary mechanism for heinous crimes. The constraint instantiates this reading's internal logic. Wrongful execution presents a mandatrophy: if execution is proportionate and morally restorative, how can it be both necessary AND occasionally error-prone? The retributive reading resolves this by treating wrongful execution as tragic error within an otherwise-valid framework — the proportionality principle stands even if procedural implementation fails. This is conceptually coherent but empirically controversial (omega: does wrongful execution invalidate the framework?). The retributive reading's ε-invariance holds: whether you measure execution's morality (philosophical metric) or its empirical consequences (deterrence), the constraint's extractiveness is high and suppression is high because the reading presupposes execution as the unique proportionate response. Alternative readings produce different ε values (deterrence_reading focuses on crime prevention metrics, abolition_reading focuses on state authority limits) — these are separate constraints.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    moral_balance_measurement,
    'How is ''moral balance'' measured or verified as restored after execution? What evidence would demonstrate that an executed offender''s death has restored victims'' families or society to a state of balance?',
    'Longitudinal study of victims'' families post-execution: psychological outcomes, sense of closure, reported restoration; compare to families where offenders received life sentences or retributive sentencing alternatives. Measurement of ''moral balance'' metric in public discourse and judicial rhetoric.',
    'If measurable and achieved: retributive reading''s extraction mechanism is justified as genuine coordination. If unmeasurable or unachieved: the ''moral balance'' claim is performative (theater_ratio rises, extractiveness may be reclassified as higher). If negatively correlated (execution increases trauma): fundamental challenge to retributive axiom.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(moral_balance_measurement, empirical, 'Whether moral balance restoration is measurable and achieved through execution').

omega_variable(
    proportionality_substitution,
    'Is execution the unique proportionate response to heinous crimes, or can alternative penalties (life imprisonment, lengthy incapacitation, restitution) provide equivalent proportionate punishment?',
    'Comparative analysis of retributive philosophy texts: do they identify proportionality principle as dependent on execution (death penalty), or do they accept alternative severe penalties as proportionate? International comparative law: jurisdictions that abolished execution and replaced it with life sentences — do their legal systems maintain retributive proportionality claims? Expert consensus in retributive jurisprudence.',
    'If execution is uniquely proportionate: retributive reading''s ε-invariance is solid; alternative readings require different structural mechanisms. If alternatives can be proportionate: execution is contingent institutional choice, not necessary to the retributive principle; ε may be reclassified as lower (constraint is coordinate mechanism plus extractive authority-preservation, not extraction per se).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_substitution, conceptual, 'Whether execution is uniquely proportionate or alternatives suffice for retributive principle').

omega_variable(
    wrongful_execution_framework_invalidation,
    'Does a demonstrated wrongful execution (later proven innocent) invalidate the retributive reading''s legitimacy, or does the reading sustain itself by treating wrongful execution as tragic error within an otherwise-valid framework?',
    'Analysis of retributive jurisprudence responses to wrongful executions (historical examples: Anthony Ray Hinton, Carlos DeLuna, etc.). Do retributive theorists revise the framework post-evidence of innocence, or do they preserve the framework by reclassifying the error as procedural rather than structural? Public discourse analysis: does evidence of wrongful execution erode retributive reading''s credibility?',
    'If framework invalidated by wrongful execution: the retributive reading is empirically contingent (depends on perfect procedure); omegas regarding procedure precision become critical gates. If framework sustains despite wrongful execution: the reading''s axioms (proportionate execution for heinous crime) are deontological rather than empirically contingent; structural disconnection between principle and practice is normalized.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(wrongful_execution_framework_invalidation, conceptual, 'Whether wrongful execution invalidates retributive framework or is treated as tragic error').

omega_variable(
    kernel_reading_foreclosure_test,
    'Does the retributive reading''s axiom (execution restores moral balance for heinous crimes) logically foreclose the abolition reading''s axiom (state execution is categorically impermissible), or do they coexist as irreducible moral disagreements?',
    'Logical analysis: can a framework hold both ''execution is categorically impermissible'' AND ''execution restores moral balance for heinous crimes''? Or does accepting proportionate execution require rejecting categorical impermissibility? Examine whether the disagreement is factual (about consequences), normative (about duties), or foundational (about the state''s legitimate authority).',
    'If foreclosure: the readings are incompatible; only one can be institutionalized. If coexistence: multiple readings can cohabitate in pluralist systems; the constraint has multiple valid ε values depending on reading adopted. Engine will compute foreclosure classification based on axiom_overriding drift and grounding_type.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_test, conceptual, 'Logical relationship between retributive and abolition readings: foreclosure vs coexistence').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(state_execution_authority__retributive_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(exec_ret_tr_t0, state_execution_authority__retributive_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(exec_ret_tr_t10, state_execution_authority__retributive_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(exec_ret_tr_t20, state_execution_authority__retributive_reading, theater_ratio, 20, 0.35).

% Extraction over time
narrative_ontology:measurement(exec_ret_be_t0, state_execution_authority__retributive_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(exec_ret_be_t10, state_execution_authority__retributive_reading, base_extractiveness, 10, 0.62).
narrative_ontology:measurement(exec_ret_be_t20, state_execution_authority__retributive_reading, base_extractiveness, 20, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(exec_ret_su_t0, state_execution_authority__retributive_reading, suppression_requirement, 0, 0.65).
narrative_ontology:measurement(exec_ret_su_t10, state_execution_authority__retributive_reading, suppression_requirement, 10, 0.7).
narrative_ontology:measurement(exec_ret_su_t20, state_execution_authority__retributive_reading, suppression_requirement, 20, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(state_execution_authority__retributive_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__deterrence_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, state_execution_authority__abolition_reading).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, victims_rights_framework).
narrative_ontology:affects_constraint(state_execution_authority__retributive_reading, proportionate_punishment_principle).

% DUAL FORMULATION NOTE:
% The state_execution_authority kernel has three reading instantiations: retributive_reading (this constraint), deterrence_reading (focuses on crime prevention), abolition_reading (focuses on categorical prohibition). Each reading has its own ε value, beneficiary/victim structure, and classification. The three constraints form a family linked by kernel identity. The retributive reading (this file) shows high ε (0.68) and snare classification from most perspectives. The deterrence reading would show lower ε if deterrence is empirically questionable. The abolition reading rejects the entire framework. All three are valid readings of the kernel; they produce different constraint stories with different structural properties.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(state_execution_authority__retributive_reading, powerless, 0.98).
constraint_indexing:directionality_override(state_execution_authority__retributive_reading, moderate, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
