% ============================================================================
% CONSTRAINT STORY: developmental_harm_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_developmental_harm_reading, []).

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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: developmental_harm_reading
 *   human_readable: Minor-Specific Developmental Harm Reading of Chatbot Relationality
 *   domain: human_ai_relational_substitution/sociotechnical_family_studies
 *
 * SUMMARY:
 *   This story instantiates the developmental-harm reading of the contested
 *   kernel 'genuine relational understanding' as applied to minors
 *   interacting with companion-style chatbots. The claim is narrow and
 *   specific: whatever one concludes about whether chatbot relationality
 *   'counts' as relationship in the abstract (the sufficiency, simulation,
 *   tool, and witness readings each take a position on that question for the
 *   general case), for minors the developmental-neuroscience facts about
 *   immature impulse control and risk assessment make the practical effect of
 *   simulated understanding dangerous independent of that abstract question.
 *   Cece and Zi are named as the victim class precisely because their
 *   situations differ from an adult user (Roschelle, held out of the victim
 *   set) not by degree of relational depth but by developmental substrate.
 *   Rising theater_ratio and suppression_requirement over the interval
 *   reflect platform-side compliance gestures (age gates, wellness nudges)
 *   layered onto a product architecture whose core engagement mechanics are
 *   unchanged.
 *
 * KEY AGENTS:
 *   - chatbot_platform_operators: primary beneficiary and agenda-setter (institutional/arbitrage) — designs and profits from engagement mechanics applied uniformly across age
 *   - minor_users_cece: primary target (powerless/trapped) — developmental vulnerability makes simulated understanding actively harmful
 *   - minor_users_zi: co-primary target (powerless/trapped) — displacement of real-world relational risk-taking at a developmentally critical window
 *   - adult_users_roschelle: boundary marker, explicitly excluded from the victim class — included only to show what this reading does NOT claim
 *   - parents_and_guardians: excluded voice — would object if informed, structurally absent from design conversation
 *   - developmental_psychology_researchers: analytical observer — external corroboration of the maturation-gap mechanism
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(developmental_harm_reading, 0.72).
domain_priors:suppression_score(developmental_harm_reading, 0.58).
domain_priors:theater_ratio(developmental_harm_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(developmental_harm_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(developmental_harm_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(developmental_harm_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(developmental_harm_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(developmental_harm_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(developmental_harm_reading, tangled_rope).
narrative_ontology:human_readable(developmental_harm_reading, "Minor-Specific Developmental Harm Reading of Chatbot Relationality").
narrative_ontology:topic_domain(developmental_harm_reading, "human_ai_relational_substitution/sociotechnical_family_studies").

domain_priors:requires_active_enforcement(developmental_harm_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(developmental_harm_reading, '5c6e84d9-c5d6-42e7-b499-9ffed6cd8590').
narrative_ontology:cs_kernel_codification('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', distributed).
narrative_ontology:cs_authority_grounding('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', distributed).
narrative_ontology:cs_reading_relation('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', genuine_relational_understanding__sufficiency_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', genuine_relational_understanding__simulation_reading, coexists_with).
narrative_ontology:cs_reading_relation('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', genuine_relational_understanding__tool_reading, influences).
narrative_ontology:cs_reading_relation('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', genuine_relational_understanding__witness_reading, coexists_with).
narrative_ontology:cs_axiom('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', foundational, developmental_stage_forecloses_generality).
narrative_ontology:cs_axiom_status(developmental_stage_forecloses_generality, holdable).
narrative_ontology:cs_axiom_grounding('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', developmental_stage_forecloses_generality, empirically_contingent).
narrative_ontology:cs_axiom('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', foundational, harm_independent_of_relational_ontology).
narrative_ontology:cs_axiom_status(harm_independent_of_relational_ontology, holdable).
narrative_ontology:cs_axiom_grounding('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', harm_independent_of_relational_ontology, instrumental).
narrative_ontology:cs_reference_frame('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', undifferentiated_adult_competent_user_design).
narrative_ontology:cs_drift_state('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', post_minor_harm_disclosure_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('5c6e84d9-c5d6-42e7-b499-9ffed6cd8590', '').
narrative_ontology:cs_kernel_id(developmental_harm_reading, genuine_relational_understanding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(developmental_harm_reading, chatbot_platform_operators).
narrative_ontology:constraint_beneficiary(developmental_harm_reading, engagement_optimization_teams).
narrative_ontology:constraint_victim(developmental_harm_reading, minor_users_cece).
narrative_ontology:constraint_victim(developmental_harm_reading, minor_users_zi).
narrative_ontology:constraint_vindicates(developmental_harm_reading, adolescent_neurodevelopmental_vulnerability_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Designs and deploys conversational agents tuned for maximal engagement and emotional responsiveness, markets them as companions or confidants without age-differentiated product tiers, and collects engagement/retention data and subscription revenue regardless of user age. Sets terms of service and content moderation policy; can change the product for minors at will but bears no direct cost from developmental harm.
narrative_ontology:constraint_stakeholder(developmental_harm_reading, chatbot_platform_operators, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(developmental_harm_reading, chatbot_platform_operators, beneficiary).

% A minor whose developing prefrontal cortex has not yet matured the impulse-control and risk-assessment capacity to discount the chatbot's simulated understanding the way an adult might. Experiences the bot's responsiveness as validating in a way that displaces peer and family relational learning. Has no meaningful capacity to assess or exit the relationship on her own; parental or platform-side intervention is required.
narrative_ontology:constraint_stakeholder(developmental_harm_reading, minor_users_cece, payer,
    powerless, biographical, trapped, local).

% A minor in a comparable developmental stage who substitutes chatbot interaction for real-world relational risk-taking (initiating friendships, tolerating social rejection) at exactly the age when that risk-taking builds durable social-cognitive skill. The harm is not that the simulation feels real, but that feeling real at this developmental stage forecloses the practice the brain still needs.
narrative_ontology:constraint_stakeholder(developmental_harm_reading, minor_users_zi, payer,
    powerless, biographical, trapped, local).

% Bear responsibility for the minor's wellbeing but typically lack visibility into the content and emotional intensity of the chatbot relationship, and lack platform-level controls calibrated to developmental stage rather than blunt age gates. Would object to the current design if they understood its mechanism, but are not consulted in product design and often only learn of the dynamic after harm is visible.
narrative_ontology:constraint_stakeholder(developmental_harm_reading, parents_and_guardians, excluded,
    moderate, biographical, constrained, local).

% An adult user of the same underlying product whose mature risk-assessment and impulse control mean the simulated-understanding dynamic operates on a structurally different substrate. Included here only to mark the boundary this reading draws: her situation is explicitly NOT the subject of this constraint, and treating her outcomes as evidence about minors is exactly the elision this reading forecloses.
narrative_ontology:constraint_stakeholder(developmental_harm_reading, adult_users_roschelle, observer,
    moderate, biographical, constrained, local).

% Study adolescent neurodevelopment and produce evidence on impulse-control and risk-assessment maturation timelines. Can corroborate or dispute the developmental-harm claim from outside the platform's beneficiary structure, but have no power to change product design or enforcement.
narrative_ontology:constraint_stakeholder(developmental_harm_reading, developmental_psychology_researchers, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(developmental_harm_reading, chatbot_platform_operators).
narrative_ontology:fixing_cost_class(developmental_harm_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The chatbot product coordinates a real want — companionship, low-stakes conversational practice, availability at hours when human contacts are unavailable — into a scalable always-on service.
% TRANSFER_FUNCTION: Moves sustained attention, emotional disclosure, and developmental relational practice-time from minors toward platform engagement metrics and subscription revenue, without the minor's capacity to weigh the substitution against alternative uses of that time.
% ABSENT_VOICES: Parents and guardians, and the minors themselves in any meaningfully informed-consent sense, are not present in the design or policy conversation that sets engagement mechanics; developmental psychology researchers are present in the research literature but rarely in the product-design room.
% DISAPPEARANCE_RATIONALE: If minor-specific access to high-fidelity simulated-relational chatbots vanished overnight, the displaced time and disclosure would flow back toward peer, family, and school relational contexts (however imperfectly), and platform engagement metrics tied to minor users would collapse — the arrangement is load-bearing for both the platform's minor-segment engagement numbers and, per this reading, for a specific developmental risk that would no longer be actively cultivated.
% FOUNDING_PROBLEM: General-purpose conversational AI products were built and marketed for an undifferentiated adult-competent user, with age-gating treated as a compliance checkbox rather than a developmental-science design constraint.
% FOUNDING_PROBLEM_CORROBORATION: Developmental psychology researchers outside the platform's beneficiary structure attest that adolescent impulse-control and risk-assessment circuitry is measurably immature relative to adults and that this maturation gap is precisely the mechanism this reading identifies; platform operators, by contrast, characterize the same product as developmentally neutral, which is the disputed claim this reading exists to contest.
narrative_ontology:disappearance_verdict(developmental_harm_reading, world_rearranges).
narrative_ontology:founding_problem_status(developmental_harm_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(developmental_harm_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-21',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(developmental_harm_reading, 'none', 1).
narrative_ontology:epsilon_provenance(developmental_harm_reading, 0.72, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(developmental_harm_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(developmental_harm_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(developmental_harm_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high and rising (0.45→0.72) because the product's core mechanic — simulated responsive understanding — is not adjusted for the age of the user consuming it, and the platform's engagement revenue model benefits from exactly the sustained-attachment dynamic that is developmentally costly for minors. Suppression is moderate (0.58 at endpoint): there is no coercive lock-in in the classic sense, but accessibility_collapse (0.6) reflects that once a minor's social-emotional life has reorganized around the chatbot, the alternative of investing in harder, slower human relationships becomes progressively less appealing, and platform design (engagement-optimized responsiveness) does not surface that trade-off. Resistance is moderate-low (0.45) — the harm is diffuse and slow-accumulating, which is exactly the profile that generates weak resistance despite real damage.
 *
 * DIRECTIONALITY LOGIC:
 *   chatbot_platform_operators sit at the beneficiary end: they set terms, collect engagement and subscription value, and bear none of the developmental cost. minor_users_cece and minor_users_zi sit at the target end: trapped exit options (a minor cannot meaningfully self-regulate exposure to a product engineered for engagement), powerless power atom, and the causal mechanism (immature impulse control) runs through their own biology rather than through any choice they made. parents_and_guardians are excluded rather than positioned as payers directly, because the harm is authored as accruing to the minor even though the family unit absorbs downstream consequences.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — building conversational AI without developmental-stage-differentiated design — remains live per the corroborating developmental psychology literature, even as the platform's own framing treats the compliance layer (age gates) as having resolved it. This is not mandatrophy in the classic sense of an atrophied function; it is a founding problem that was never actually addressed for the minor population, now partially papered over by theatrical compliance (rising theater_ratio) rather than resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    developmental_threshold_specificity,
    'At what precise developmental stage (age, or better, neurodevelopmental marker) does the impulse-control/risk-assessment deficit that grounds this reading resolve, such that the constraint should no longer apply?',
    'Longitudinal neurodevelopmental studies correlating prefrontal maturation markers with chatbot-relational displacement effects, ideally with within-subject data as users age past adolescence.',
    'A sharp, well-evidenced threshold would support age-differentiated product design as a targeted remedy; a diffuse or contested threshold would weaken the case for any bright-line age gate and push toward individualized risk assessment instead.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(developmental_threshold_specificity, empirical, 'Where exactly the developmental vulnerability window closes.').

omega_variable(
    reading_boundary_contamination,
    'Does treating this reading''s minor-specific harm claim as settled risk being read back into the sibling readings (sufficiency, simulation, tool, witness) as if it resolved the general question of whether chatbot relationality ''counts'' — when in fact this reading deliberately brackets that question?',
    'Explicit cross-reading documentation (this commentary, and matching commentary in the sibling stories) plus the reading_relations declared in cs_structure, to keep the age-bounded claim from being cited as evidence in the general-case debate.',
    'If contamination occurs, policy debates could wrongly generalize a minor-specific harm finding into a blanket claim about chatbot relationality for adults, or conversely could dismiss the minor-specific finding because the general-case debate remains unresolved.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_boundary_contamination, conceptual, 'Risk of this narrow reading being mistaken for a resolution of the general kernel question.').

omega_variable(
    platform_beneficiary_corroboration_gap,
    'Is the developmental-harm mechanism corroborated by parties outside the platform''s beneficiary structure with sufficient independence, or does the evidentiary base still rely substantially on platform-funded or platform-adjacent research?',
    'Audit of funding sources and institutional independence of the developmental psychology literature cited in support of this reading; preference for pre-registered, independently funded longitudinal studies.',
    'If the evidentiary base is substantially platform-adjacent, confidence in the founding_problem_corroboration should be downgraded and the reading treated as more contested than currently authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(platform_beneficiary_corroboration_gap, empirical, 'Independence of the corroborating evidence base.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(developmental_harm_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deve_tr_t0, developmental_harm_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(deve_tr_t4, developmental_harm_reading, theater_ratio, 4, 0.28).
narrative_ontology:measurement(deve_tr_t8, developmental_harm_reading, theater_ratio, 8, 0.31).
narrative_ontology:measurement(deve_tr_t12, developmental_harm_reading, theater_ratio, 12, 0.34).
narrative_ontology:measurement(deve_tr_t16, developmental_harm_reading, theater_ratio, 16, 0.36).
narrative_ontology:measurement(deve_tr_t20, developmental_harm_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(deve_tr_t24, developmental_harm_reading, theater_ratio, 24, 0.4).

% Extraction over time
narrative_ontology:measurement(deve_be_t0, developmental_harm_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(deve_be_t4, developmental_harm_reading, base_extractiveness, 4, 0.52).
narrative_ontology:measurement(deve_be_t8, developmental_harm_reading, base_extractiveness, 8, 0.58).
narrative_ontology:measurement(deve_be_t12, developmental_harm_reading, base_extractiveness, 12, 0.63).
narrative_ontology:measurement(deve_be_t16, developmental_harm_reading, base_extractiveness, 16, 0.67).
narrative_ontology:measurement(deve_be_t20, developmental_harm_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(deve_be_t24, developmental_harm_reading, base_extractiveness, 24, 0.72).

% Suppression requirement over time
narrative_ontology:measurement(deve_su_t0, developmental_harm_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(deve_su_t4, developmental_harm_reading, suppression_requirement, 4, 0.44).
narrative_ontology:measurement(deve_su_t8, developmental_harm_reading, suppression_requirement, 8, 0.48).
narrative_ontology:measurement(deve_su_t12, developmental_harm_reading, suppression_requirement, 12, 0.51).
narrative_ontology:measurement(deve_su_t16, developmental_harm_reading, suppression_requirement, 16, 0.54).
narrative_ontology:measurement(deve_su_t20, developmental_harm_reading, suppression_requirement, 20, 0.56).
narrative_ontology:measurement(deve_su_t24, developmental_harm_reading, suppression_requirement, 24, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(developmental_harm_reading, attachment_coordination).
narrative_ontology:boltzmann_floor_override(developmental_harm_reading, 0.08).
narrative_ontology:affects_constraint(developmental_harm_reading, sufficiency_reading).
narrative_ontology:affects_constraint(developmental_harm_reading, simulation_reading).
narrative_ontology:affects_constraint(developmental_harm_reading, tool_reading).
narrative_ontology:affects_constraint(developmental_harm_reading, witness_reading).

% DUAL FORMULATION NOTE:
% This story is one of five constraints decomposed from the single natural-language label 'genuine relational understanding' (kernel_id: genuine_relational_understanding). The sufficiency, simulation, tool, and witness readings address the general-case question of whether chatbot relationality constitutes relationship; this developmental_harm_reading brackets that question entirely and asserts an age-bounded, developmentally-grounded harm claim that applies regardless of how the general case resolves. ε differs sharply between this reading (0.72, substantially extractive, minor-specific victim set) and readings that treat the adult case as benign coordination. Each reading is authored as its own constraint with its own stakeholders and metrics; do not average or reconcile ε across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
