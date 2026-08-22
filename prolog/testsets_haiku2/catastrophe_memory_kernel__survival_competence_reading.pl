% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Catastrophe-Memory Ritual: Survival-Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint is ONE READING of the catastrophe-memory kernel — the
 *   survival-competence reading. The kernel itself is a contested religious
 *   and cultural practice: mourning and remembrance rituals performed by
 *   persecuted communities. This reading frames the ritual as a mechanism for
 *   encoding and transmitting the practical, operational competence required
 *   to survive organized persecution. Under this reading, the ritual is
 *   neither primarily symbolic continuity (the symbol-continuity reading) nor
 *   primarily trauma encoding (the trauma-encoding reading) nor primarily
 *   boundary maintenance (the boundary-maintenance reading), though it may
 *   serve those functions secondarily. The primary function here is survival
 *   training: the ritual teaches clandestine gathering, resource concealment,
 *   infiltration detection, safe-house networks, recognition signals, and
 *   crisis response through embodied, intergenerational transmission. The
 *   beneficiary is community resilience under threat; the victim is the cost
 *   borne by those pressured to assimilate or who experience the rituals as
 *   empty obligation. The constraint is CLAIMED as tangled_rope (coordination
 *   function for survival competence, but asymmetric extraction from those
 *   locked into participation) and AUTHORED with moderate extractiveness
 *   (0.48): the coordination problem is genuine, but enforcement coerces
 *   participation from skeptics and creates identity fusion that makes exit
 *   costly.
 *
 * KEY AGENTS:
 *   - threatened_community: Persecuted minority group that benefits from distributed, embodied survival knowledge — organized power, generational time horizon, identity-locked exit.
 *   - ritual_custodians: Elders or authorized transmitters who maintain and enforce the ritual structure — organized power, constrained exit (guardianship role is part of their identity).
 *   - assimilated_members: Community-born individuals under sustained pressure to abandon rituals and adopt dominant-culture practices — moderate power, constrained exit (exit means assimilation but remaining means bearing visibility costs).
 *   - secular_descendants: Identity-locked community members who do not believe in the religious framework but must perform the rituals for group coherence and security — moderate power, identity-locked exit (cannot leave the community without profound rupture).
 *   - persecution_authority: State or dominant-group actors actively disrupting the rituals through infiltration, prosecution, or forced apostasy — institutional power, trapped in opposition to community survival.
 *   - external_sympathizers: Researchers, diaspora advocates, human-rights observers — moderate power, mobile exit (can withdraw support if political context shifts).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.48).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.62).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Catastrophe-Memory Ritual: Survival-Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '6e1c898a-865d-4c3e-8536-00c2ac50e350').
narrative_ontology:cs_kernel_codification('6e1c898a-865d-4c3e-8536-00c2ac50e350', distributed).
narrative_ontology:cs_authority_grounding('6e1c898a-865d-4c3e-8536-00c2ac50e350', practice).
narrative_ontology:cs_interpretation_layer_present('6e1c898a-865d-4c3e-8536-00c2ac50e350').
narrative_ontology:cs_reading_relation('6e1c898a-865d-4c3e-8536-00c2ac50e350', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e1c898a-865d-4c3e-8536-00c2ac50e350', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('6e1c898a-865d-4c3e-8536-00c2ac50e350', catastrophe_memory_kernel__trauma_encoding_reading, influences).
narrative_ontology:cs_axiom('6e1c898a-865d-4c3e-8536-00c2ac50e350', foundational, survival_competence_as_primary_function).
narrative_ontology:cs_axiom_status(survival_competence_as_primary_function, holdable).
narrative_ontology:cs_axiom_grounding('6e1c898a-865d-4c3e-8536-00c2ac50e350', survival_competence_as_primary_function, empirically_contingent).
narrative_ontology:cs_axiom('6e1c898a-865d-4c3e-8536-00c2ac50e350', foundational, ritual_as_distributed_resistance_training).
narrative_ontology:cs_axiom_status(ritual_as_distributed_resistance_training, holdable).
narrative_ontology:cs_axiom_grounding('6e1c898a-865d-4c3e-8536-00c2ac50e350', ritual_as_distributed_resistance_training, instrumental).
narrative_ontology:cs_reference_frame('6e1c898a-865d-4c3e-8536-00c2ac50e350', persecuted_community_under_active_threat).
narrative_ontology:cs_drift_state('6e1c898a-865d-4c3e-8536-00c2ac50e350', diaspora_or_threat_reduction_context, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('6e1c898a-865d-4c3e-8536-00c2ac50e350', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, threatened_community).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilated_members).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, secular_descendants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participates in mourning and remembrance rituals that encode specific survival strategies: clandestine gathering protocols, resource concealment practices, resistance tactics, recognition signals for trusted allies. The rituals rehearse catastrophic scenarios and encode responses through narrative, gesture, and temporal pacing. Members who perform the rituals develop practical competence in crisis response and maintain group coherence under existential threat.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, threatened_community, beneficiary,
    organized, generational, identity_locked, national).

% Maintain, transmit, and enforce the ritual structure across generations. They decide what aspects of survival strategy are encoded, which contingencies are rehearsed, and how the rituals evolve to meet new threats. They have authority to exclude those who do not perform correctly or who breach operational security by exposing protocols.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, ritual_custodians, agenda_setter,
    organized, generational, constrained, national).

% Face sustained social and economic pressure to abandon ritual participation and adopt dominant-culture practices. They must choose between maintaining group identity (and bearing the cost of ritual participation, social marginaldom, and visibility to persecution authorities) or exiting toward assimilation. Exiting breaks the chain of competence transmission and exposes remaining community members by reducing available safe houses, economic networks, and distributed knowledge of hiding places.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilated_members, payer,
    moderate, biographical, constrained, national).

% Are born into the community but do not believe in the religious framework that originally grounded the rituals. They face enforcement pressure to participate anyway — performing rituals they experience as empty, theatrical, or obsolete. Their participation is mandatory for group coherence, operational security (gaps in ritual networks create vulnerabilities), and transmission of practical survival knowledge. The rituals feel extractive to them: they pay in time, attention, and authentic feeling to maintain something whose rationale they do not share.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, secular_descendants, payer,
    moderate, biographical, identity_locked, national).

% Actively seeks to disrupt the ritual structure by infiltrating ceremonies, prosecuting participants, or using forced apostasy requirements to break transmission chains. They understand that the rituals encode practical resistance strategy, not merely symbolic continuity. Their exclusion from the ceremonies means their countermeasures remain incomplete — they cannot fully decode the survival knowledge the rituals contain.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecution_authority, excluded,
    institutional, generational, trapped, national).

% Academic researchers, diaspora members, human-rights advocates observing from outside the immediate threat zone. They document the rituals, provide material support, and advocate for the community's protection. They can exit the engagement when the political context shifts but carry no operational responsibility for community survival.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, external_sympathizers, observer,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__survival_competence_reading, ritual_custodians).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__survival_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Encodes and rehearses specific adaptive responses to existential threat — gathering protocols, resource networks, safe-house identification, infiltration detection, children's concealment — in a format that survives suppression and transmits across generations even when formal teaching is impossible. The ritual pacing, narrative structure, and bodily repetition create distributed memory that persists despite persecution, literacy suppression, and intergenerational displacement.
% TRANSFER_FUNCTION: Moves the burden of survival-knowledge maintenance and ritual performance from those who could exit (the assimilated or secular) to those locked into the community by identity or circumstance. The community collects practical competence; the payers bear the cost of visibility to authorities, time spent in ritual rather than economic advancement, and identity fusion that makes exit traumatic.
% ABSENT_VOICES: Persecuting authorities who actively work to disrupt the rituals are structurally excluded from design decisions; they would argue for ritual suppression as a public-safety measure. Victims of failed escape attempts (those whose safe houses were discovered because network knowledge was incomplete when a trained member assimilated) cannot testify but their deaths inform the ritual's operational seriousness.
% DISAPPEARANCE_RATIONALE: If the catastrophe-memory rituals vanished, community members would lose the distributed, embodied knowledge of how to survive organized persecution. Practical competence would collapse into individual improvisation. Persecution authorities would face no coordinated resistance and would consolidate control more rapidly. The community's long-term survival capacity would shift from high to critically dependent on external protection or the constraint's reinvention.
% FOUNDING_PROBLEM: How does a persecuted community preserve and transmit survival competence when formal education is forbidden, literacy is suppressed, written records are confiscated, and every public gathering risks arrest? How do children learn evasion techniques, resource networks, and group-recognition signals when they cannot attend school or ask questions?
% FOUNDING_PROBLEM_CORROBORATION: Historical and contemporary persecution records document active disruption of minority-community rituals (compulsory attendance at dominant-religion ceremonies, bans on native-language prayer, infiltration of gathering spaces). Survivor testimonies from multiple persecuted groups (Jewish communities under medieval and modern persecution, Uighur communities under contemporary suppression, Christian communities in persecuting regimes) identify ritual as a key to survival. Security studies literature on insurgency and clandestine organization recognizes ritual rehearsal as a standard tactical training mechanism for communities under threat. The founding problem is attested from outside the ritual community itself — by persecutors who target the rituals as operationally significant, by external researchers, and by survivors describing what the rituals taught them.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.48, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48) because the constraint has a genuine coordination function — encoding survival competence — but that function is shadowed by enforcement coercion on skeptics and by the identity-lock mechanism that prevents exit. The measurement series shows slight upward drift in extractiveness (0.38 → 0.48) as secular descendants internalize the rituals' necessity and the constraints on exiting become clearer; theater ratio (~0.40) is stable and moderate, indicating that the ritual is functional (not purely performative) but has a theatrical layer for security (disguising tactical content as religious observance). Suppression requirement (0.51 → 0.62) rises modestly over the interval as persecution authorities intensify efforts to disrupt the rituals, requiring stronger internal discipline and secrecy. The coercion_grid shows that individual-level stakes inflation (0.78) is highest because the personal cost of exposure or failure is acute; organizational and class-level suppression (0.52–0.60) is lower because the community maintains dense networks that distribute enforcement costs. Resistance grows over the interval (individual 0.38 → 0.41, organizational 0.58 → 0.61, class-level 0.62 → 0.65) as younger members find external allies and question the constraints' necessity — though the rise is modest, indicating the identity-lock mechanism and persecution threat keep defection manageable. The constraint is neither pure rope (coordination would be voluntary if the threat ceased) nor pure snare (genuine skill transmission occurs) — it is tangled rope: coordination function + asymmetric extraction from those without exit options.
 *
 * PERSPECTIVAL GAP:
 *   The ritual_custodian seat and the threatened_community beneficiary seat should compute as beneficiary-side; the assimilated and secular-descendant seats should compute as target-side. From the custodian's position, the rituals are genuine survival training they maintain at real cost and pass to those who understand their necessity. From the secular-descendant's position, the same rituals are coercive performativity — they are forced to participate in something whose rationale they reject, paying in authenticity and time to maintain group coherence. The engine should compute this divergence from directionality derivation: beneficiaries get low d (subsidized by the constraint) and targets get high d (extracted from); identity_locked exit for both groups pushes targets further into the extraction zone. The boundary is permeable — some secular descendants come to understand the rituals' necessity and shift toward the beneficiary frame; some assimilated members experience their exit as partial and bear lingering costs. The per-seat classification should capture this variation.
 *
 * DIRECTIONALITY LOGIC:
 *   The threatened_community and ritual_custodians are the structural beneficiaries (they collect practical survival knowledge and can make informed decisions about ritual participation) — they get low directionality (d near 0.1–0.3). Assimilated members face constrained exit (assimilation is available but costs identity and family rupture) — they get moderate d (0.4–0.6). Secular descendants are most constrained: they cannot exit the community (identity-locked) but must participate in rituals whose rationale they do not share; they get high d (0.7–0.9). The persecution authority is structurally opposed to the constraint's operation — it is not a beneficiary or victim in the internal calculus but an external force that shapes how high the participation cost must remain. The key directionality driver is identity-lock: both community members and outsiders understand that exit from the community means severing primary bonds, economic networks, and self-concept. That fusion keeps the enforcement load lower than external suppression would be — the constraint is self-maintaining in part through internalized identity cost.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (how to preserve survival competence under persecution) is LIVE — persecution of the communities that maintain these rituals continues in contemporary contexts. The constraint's function is NOT obsolete. However, mandatrophy ambiguity arises on two axes: (1) In diaspora and low-persecution contexts, the constraint's survival-training function atrophies — the rituals persist as culture and identity but the competence they encode (evasion of local authorities, resource networks for hidden movement, infiltration detection) becomes ceremonial. (2) For secular descendants and assimilated members, the constraint's function is already partially dead — they do not believe the survival competence is necessary, and enforcement coerces their participation despite their judgment that the founding problem is solved (at least in their local context). The mandatrophy_analysis distinguishes functional from performative: in high-persecution zones, the rituals remain functionally extractive (coercing participation from skeptics is justified by the real threat); in low-persecution zones, the same rituals shift toward piton-type operation (theater replaces function, but enforcement persists because the community's identity now depends on them). This distinction is contextual and per-seat. The classification mechanism should flag the constraint as live-in-context (mandatrophy not resolved) but note the structural instability: when the threat decreases, the extraction becomes harder to justify and the constraint slides toward piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ritual_function_vs_form,
    'To what extent does the survival competence encoding depend on the specific religious or symbolic form of the rituals, versus residing in the procedural structure that could be transmitted in non-ritual format?',
    'Anthropological case studies of communities that have transitioned to secular ritual formats while maintaining survival-competence transmission; comparison of ritual-transmitted knowledge against explicitly taught equivalent knowledge in contexts where ritual is suppressed.',
    'If the competence is primarily procedural and the religious form is optional, then the constraint''s extraction (requiring participation in religious observance) could be substantially reduced by decoupling transmission from ritual form. If the form is constitutive of the competence (i.e., embodied practice in sacred context is how the knowledge sticks and survives), then the extraction is structural, not optional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ritual_function_vs_form, empirical, 'Whether survival competence depends on the specific ritual form or on the procedural content.').

omega_variable(
    secular_descendant_internalization,
    'Do secular descendants over time internalize the rituals'' necessity (shift their judgment as they experience the founding problem as live), or do they resist and strategically comply while maintaining disbelief?',
    'Longitudinal interviews with secular-descendant cohorts; observation of whether resistance increases or decreases with age and experience; whether ritual reformulation (modernizing the language/content while keeping the structure) reduces resistance or is understood as delegitimization.',
    'If they internalize necessity, the constraint becomes lower-extractiveness over time (suppression decreases because coercion is replaced by consent); if they resist while complying, the constraint remains extractive and theater-ratio rises (compliance becomes performative). This affects whether the constraint should compute as tangled_rope (coordination + extraction) or as transitioning toward piton (theater replacing function).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_descendant_internalization, empirical, 'Whether secular descendants'' resistance to ritual participation is persistent or situationally reversible through experience.').

omega_variable(
    context_dependency_of_persecution,
    'Is the constraint''s extractiveness fundamentally context-dependent — high in active-persecution zones, degrading toward theater in diaspora/low-threat zones? If so, is a single ε value adequate, or should this be decomposed into multiple constraints per context?',
    'Ethnographic comparison across diaspora vs. in-country communities; temporal analysis of the same community as threat level changes; measurement of belief alignment with threat perception.',
    'If context-dependent, the single ε value (0.48) homogenizes what are structurally different constraints: the survival-training function is real and high-ε in persecution zones, but in diaspora the same rituals shift toward piton. The constraint should either be explicitly authored as context-dependent (with measurements reflecting the divergence) or decomposed per ε-invariance rules into separate stories — one for survival_competence_in_active_persecution, one for survival_competence_in_diaspora. The current story assumes moderate extractiveness across contexts; if contexts differ substantially, the story''s ε claim becomes imprecise.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(context_dependency_of_persecution, conceptual, 'Whether the constraint''s ε and function are invariant across persecution-threat contexts or require decomposition.').

omega_variable(
    reading_foreclosure_probability,
    'Does the survival_competence_reading logically foreclose the symbol_continuity_reading? Or do they coexist — can both be true simultaneously under different descriptions of the same ritual?',
    'Axiomatic analysis: does survival-competence focus require denial of symbolic continuity, or are they orthogonal? Historical analysis: have communities that explicitly adopted the survival-competence frame also maintained symbolic continuity, or are they mutually exclusive?',
    'If they coexist (both are true simultaneously), the reading_relations are coexists_with for all siblings. If this reading''s core premise (ritual primarily encodes operational competence) logically excludes the symbol_continuity premise (ritual primarily preserves symbolic meaning across time), then reading_relations should include forecloses. The decision affects how the readings sit in the kernel structure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_foreclosure_probability, conceptual, 'Structural relationship between survival_competence_reading and sibling readings in the kernel.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression (0.62) primarily structural (persecution authorities actively disrupt the rituals, physical risk of arrest) or internalized (members self-censor out of fear, even when no actual enforcement action is imminent)?',
    'Post-threat-reduction measurement: if suppression persists after official persecution authority activity decreases (but community members still fear), suppression is internalized. If suppression collapses when external threat recedes, it is structural. Comparative study: communities with active persecution vs. communities where persecution is historical but not current.',
    'If suppression is primarily internalized, the constraint carries its coercive force within the community structure itself; exit costs are psychological and relational rather than physical. If primarily structural, removal of the external threat could reduce the constraint''s extractiveness. For secular descendants, internalized suppression means they carry the constraint even in safe contexts (higher identity-lock); structural suppression means they could exit if persecution stopped.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the constraint.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.35).
narrative_ontology:measurement_basis(cata_tr_t0, observed).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 10, 0.37).
narrative_ontology:measurement_basis(cata_tr_t10, observed).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(cata_tr_t20, observed).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement_basis(cata_tr_t30, observed).
narrative_ontology:measurement(cata_tr_t45, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 45, 0.41).
narrative_ontology:measurement_basis(cata_tr_t45, observed).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 60, 0.41).
narrative_ontology:measurement_basis(cata_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement_basis(cata_be_t0, observed).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 10, 0.42).
narrative_ontology:measurement_basis(cata_be_t10, observed).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.46).
narrative_ontology:measurement_basis(cata_be_t20, observed).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 30, 0.49).
narrative_ontology:measurement_basis(cata_be_t30, observed).
narrative_ontology:measurement(cata_be_t45, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 45, 0.47).
narrative_ontology:measurement_basis(cata_be_t45, observed).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 60, 0.48).
narrative_ontology:measurement_basis(cata_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.51).
narrative_ontology:measurement_basis(cata_su_t0, observed).
narrative_ontology:measurement(cata_su_t10, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 10, 0.56).
narrative_ontology:measurement_basis(cata_su_t10, observed).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(cata_su_t20, observed).
narrative_ontology:measurement(cata_su_t30, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 30, 0.63).
narrative_ontology:measurement_basis(cata_su_t30, observed).
narrative_ontology:measurement(cata_su_t45, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 45, 0.62).
narrative_ontology:measurement_basis(cata_su_t45, observed).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 60, 0.62).
narrative_ontology:measurement_basis(cata_su_t60, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=60
narrative_ontology:measurement(cata_grid_01, catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse(class), 0, 0.65).
narrative_ontology:measurement(cata_grid_02, catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse(class), 60, 0.63).
narrative_ontology:measurement(cata_grid_03, catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse(individual), 0, 0.72).
narrative_ontology:measurement(cata_grid_04, catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse(individual), 60, 0.71).
narrative_ontology:measurement(cata_grid_05, catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse(organizational), 0, 0.68).
narrative_ontology:measurement(cata_grid_06, catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse(organizational), 60, 0.66).
narrative_ontology:measurement(cata_grid_07, catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse(structural), 0, 0.62).
narrative_ontology:measurement(cata_grid_08, catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse(structural), 60, 0.59).
narrative_ontology:measurement(cata_grid_09, catastrophe_memory_kernel__survival_competence_reading, resistance(class), 0, 0.62).
narrative_ontology:measurement(cata_grid_10, catastrophe_memory_kernel__survival_competence_reading, resistance(class), 60, 0.65).
narrative_ontology:measurement(cata_grid_11, catastrophe_memory_kernel__survival_competence_reading, resistance(individual), 0, 0.38).
narrative_ontology:measurement(cata_grid_12, catastrophe_memory_kernel__survival_competence_reading, resistance(individual), 60, 0.41).
narrative_ontology:measurement(cata_grid_13, catastrophe_memory_kernel__survival_competence_reading, resistance(organizational), 0, 0.58).
narrative_ontology:measurement(cata_grid_14, catastrophe_memory_kernel__survival_competence_reading, resistance(organizational), 60, 0.61).
narrative_ontology:measurement(cata_grid_15, catastrophe_memory_kernel__survival_competence_reading, resistance(structural), 0, 0.44).
narrative_ontology:measurement(cata_grid_16, catastrophe_memory_kernel__survival_competence_reading, resistance(structural), 60, 0.47).
narrative_ontology:measurement(cata_grid_17, catastrophe_memory_kernel__survival_competence_reading, stakes_inflation(class), 0, 0.68).
narrative_ontology:measurement(cata_grid_18, catastrophe_memory_kernel__survival_competence_reading, stakes_inflation(class), 60, 0.67).
narrative_ontology:measurement(cata_grid_19, catastrophe_memory_kernel__survival_competence_reading, stakes_inflation(individual), 0, 0.78).
narrative_ontology:measurement(cata_grid_20, catastrophe_memory_kernel__survival_competence_reading, stakes_inflation(individual), 60, 0.76).
narrative_ontology:measurement(cata_grid_21, catastrophe_memory_kernel__survival_competence_reading, stakes_inflation(organizational), 0, 0.72).
narrative_ontology:measurement(cata_grid_22, catastrophe_memory_kernel__survival_competence_reading, stakes_inflation(organizational), 60, 0.71).
narrative_ontology:measurement(cata_grid_23, catastrophe_memory_kernel__survival_competence_reading, stakes_inflation(structural), 0, 0.55).
narrative_ontology:measurement(cata_grid_24, catastrophe_memory_kernel__survival_competence_reading, stakes_inflation(structural), 60, 0.54).
narrative_ontology:measurement(cata_grid_25, catastrophe_memory_kernel__survival_competence_reading, suppression(class), 0, 0.52).
narrative_ontology:measurement(cata_grid_26, catastrophe_memory_kernel__survival_competence_reading, suppression(class), 60, 0.53).
narrative_ontology:measurement(cata_grid_27, catastrophe_memory_kernel__survival_competence_reading, suppression(individual), 0, 0.68).
narrative_ontology:measurement(cata_grid_28, catastrophe_memory_kernel__survival_competence_reading, suppression(individual), 60, 0.67).
narrative_ontology:measurement(cata_grid_29, catastrophe_memory_kernel__survival_competence_reading, suppression(organizational), 0, 0.59).
narrative_ontology:measurement(cata_grid_30, catastrophe_memory_kernel__survival_competence_reading, suppression(organizational), 60, 0.6).
narrative_ontology:measurement(cata_grid_31, catastrophe_memory_kernel__survival_competence_reading, suppression(structural), 0, 0.48).
narrative_ontology:measurement(cata_grid_32, catastrophe_memory_kernel__survival_competence_reading, suppression(structural), 60, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.12).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_kernel is instantiated as four separate constraint stories, one per reading. Each reading names a different primary function (survival competence, symbolic continuity, trauma encoding, boundary maintenance) and should compute as a distinct constraint type from its siblings. This reading (survival_competence_reading) frames the kernel as a tangled_rope for survival training; the boundary_maintenance_reading frames it as enforcement of group identity; the symbol_continuity_reading frames it as cultural preservation; the trauma_encoding_reading frames it as intergenerational warning. All four readings share the same referent (the actual mourning practices) but author different ε values because they assess different functions as the constraint's primary purpose. The readings coexist as live positions held by different community members and external scholars.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
