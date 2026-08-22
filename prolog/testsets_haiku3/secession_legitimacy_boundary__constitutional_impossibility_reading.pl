% ============================================================================
% CONSTRAINT STORY: secession_legitimacy_boundary__constitutional_impossibility_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_secession_legitimacy_boundary__constitutional_impossibility_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: secession_legitimacy_boundary__constitutional_impossibility_reading
 *   human_readable: Constitutional Impossibility of Unilateral Secession
 *   domain: political_economy/federalism
 *
 * SUMMARY:
 *   This constraint instantiates one reading of the contested kernel
 *   'secession_legitimacy_boundary': the constitutional_impossibility reading
 *   asserts that unilateral secession is categorically impermissible under
 *   the constitution; only negotiated exit via formal constitutional
 *   amendment is legitimate. This reading treats the constitutional text as
 *   settling the question of secession legitimacy and treats all alternatives
 *   (popular_sovereignty_reading, grievance_threshold_reading,
 *   treaty_primacy_reading) as constitutionally foreclosed or merely
 *   politically defeated. The federal authority (central state,
 *   constitutional court) enforces this reading by repeatedly ruling
 *   secession bids unconstitutional. Regional majorities seeking exit face
 *   categorical delegitimization under this reading — their claims are ruled
 *   out of constitutional bounds, not contested on merits. The constraint is
 *   CLAIMED as a mountain (emerges naturally from the structure of federalism
 *   itself), but the presence of identifiable beneficiaries
 *   (federal_authority, constitutional_court) and the active enforcement
 *   machinery (suppression: 0.71, requiring_active_enforcement: true per the
 *   measurement series) suggest a false-summit candidate. The claim/metric
 *   gap is intentional and diagnostically meaningful: a claimed mountain that
 *   requires substantial enforcement and has concentrated beneficiaries is
 *   exactly the structure false-summit detection targets.
 *
 * KEY AGENTS:
 *   - Federal authority: enforces the constitutional impossibility reading, interprets secession claims as categorically illegitimate, maintains the amendment-only exit pathway, derives institutional legitimacy from constitutional supremacy doctrine
 *   - Provincial majority seeking exit: structured as identity-locked payer, bears the cost of categorical delegitimization, cannot exit through the democratic channel they perceive as legitimate (referendum)
 *   - Constitutional court: adjudicates and enforces the reading, derives institutional power from the constraint's persistence, benefits from remaining the final arbiter of legitimacy claims
 *   - Separatist movements: mobilize around exit claims but face categorical legal delegitimization under this reading, bear suppression costs of being excluded from legitimate political discourse
 *   - Federal minorities: benefit from territorial preservation, protected by the rule because exit of a region would reduce their power
 *   - Indigenous treaty holders: systematically excluded from the secession legitimacy conversation despite holding treaty-based territorial claims that cross-cut both federal and provincial frameworks
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.62).
domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.71).
domain_priors:theater_ratio(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, 0.78).
narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, 0.59).

% --- Constraint claim ---
narrative_ontology:constraint_claim(secession_legitimacy_boundary__constitutional_impossibility_reading, mountain).
narrative_ontology:human_readable(secession_legitimacy_boundary__constitutional_impossibility_reading, "Constitutional Impossibility of Unilateral Secession").
narrative_ontology:topic_domain(secession_legitimacy_boundary__constitutional_impossibility_reading, "political_economy/federalism").

domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(secession_legitimacy_boundary__constitutional_impossibility_reading, 'd0615fe6-f312-42d6-bdfc-87dd413b2b9e').
narrative_ontology:cs_kernel_codification('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', fixed_text).
narrative_ontology:cs_authority_grounding('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', extraction).
narrative_ontology:cs_interpretation_layer_present('d0615fe6-f312-42d6-bdfc-87dd413b2b9e').
narrative_ontology:cs_reading_relation('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', secession_legitimacy_boundary__popular_sovereignty_reading, forecloses).
narrative_ontology:cs_reading_relation('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', secession_legitimacy_boundary__grievance_threshold_reading, forecloses).
narrative_ontology:cs_reading_relation('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', secession_legitimacy_boundary__treaty_primacy_reading, coexists_with).
narrative_ontology:cs_axiom('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', foundational, constitutional_text_supremacy_over_regional_democracy).
narrative_ontology:cs_axiom_status(constitutional_text_supremacy_over_regional_democracy, holdable).
narrative_ontology:cs_axiom_grounding('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', constitutional_text_supremacy_over_regional_democracy, conventional).
narrative_ontology:cs_axiom('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', foundational, amendment_requirement_precludes_unilateral_exit).
narrative_ontology:cs_axiom_status(amendment_requirement_precludes_unilateral_exit, holdable).
narrative_ontology:cs_axiom_grounding('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', amendment_requirement_precludes_unilateral_exit, deontological).
narrative_ontology:cs_reference_frame('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', constitutional_territorial_integrity).
narrative_ontology:cs_drift_state('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', contemporary_persistent_exit_sentiment, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d0615fe6-f312-42d6-bdfc-87dd413b2b9e', '2026-06-12T14:32:00Z').
narrative_ontology:cs_kernel_id(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_authority).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_integrity_doctrine).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_court).
narrative_ontology:constraint_beneficiary(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_minorities).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, provincial_majority_seeking_exit).
narrative_ontology:constraint_victim(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movements).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_supremacy).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, territorial_indivisibility).
narrative_ontology:constraint_vindicates(secession_legitimacy_boundary__constitutional_impossibility_reading, hierarchical_federalism).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The central state apparatus, backed by constitutional courts and enforcement capacity. Interprets and enforces the constitutional rule that secession is impermissible absent formal amendment. Maintains the constitutional text as the binding authority structure and adjudicates all legitimacy claims against it. Bears no cost from the constraint; derives legitimacy from upholding constitutional supremacy.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_authority, agenda_setter,
    institutional, generational, analytical, national).

% A regional population with strong majoritarian support for secession who frame their exit claim as rooted in democratic self-determination. Under this reading, their claim is constitutionally categorically illegitimate regardless of the strength of grievance or democratic consensus. Their exit avenue exists only through the formal constitutional amendment process, which requires federal consent — consent structurally unlikely when the exit would diminish federal territory and authority. They cannot exit through the channel they perceive as legitimate (referendum/democratic will); they face structural closure of their preferred exit path.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, provincial_majority_seeking_exit, payer,
    organized, biographical, identity_locked, regional).

% The judicial authority that interprets and enforces the constitutional text. Acts as the arbiter of legitimacy claims, repeatedly ruling secession bids unconstitutional when challenged. Derives institutional authority and relevance from the constraint's persistence — dissolving the constraint would reduce the court's gatekeeping power over territorial questions.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_court, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(secession_legitimacy_boundary__constitutional_impossibility_reading, constitutional_court, beneficiary).

% Political organizations advocating exit. Face categorical delegitimization under this reading — their claims are ruled out of bounds, not contested on merits. They bear the cost of being excluded from legitimate political discourse, with their mobilization efforts characterized as constitutionally futile. They remain trapped in the federation's authority structure with no formally-recognized exit pathway.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, separatist_movements, payer,
    moderate, biographical, identity_locked, regional).

% Minorities at the federal level benefit from the rule because secession of a region would reduce their political power and resources within remaining territory. The constraint protects their position by making exit structurally unavailable regardless of regional majorities' preferences. Their consent is never sought but their interests are served.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_minorities, beneficiary,
    moderate, biographical, constrained, national).

% Foreign governments and international law scholars who track the constraint's operation and its enforceability. Can offer comparative analysis of constitutional secession rules in other federations, recognition decisions on exit attempts, and evidence about whether unilateral exit bids survive international legal challenge.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, international_observers, observer,
    institutional, generational, analytical, global).

% Indigenous nations whose treaty authority predates and cross-cuts both the federal system and any regional secession claim. Systematically excluded from the secession legitimacy debate despite holding treaty-based territorial and governance claims that would be directly affected by either federal preservation or regional exit. Their voice and authority framework are not recognized by this constraint.
narrative_ontology:constraint_stakeholder(secession_legitimacy_boundary__constitutional_impossibility_reading, indigenous_treaty_holders, excluded,
    moderate, generational, trapped, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(secession_legitimacy_boundary__constitutional_impossibility_reading, federal_authority).
narrative_ontology:fixing_cost_class(secession_legitimacy_boundary__constitutional_impossibility_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Stabilizes federal territorial integrity by establishing a single, determinate constitutional rule for exit legitimacy: unilateral exit is off the table; only negotiated constitutional amendment (requiring broad consensus including federal authorities) can alter territorial boundaries. This prevents individual regions from exiting unilaterally and dissolving the federal union by cascading defection.
% TRANSFER_FUNCTION: Moves legitimacy authority from provincial democratic majorities (who claim the right to exit via referendum) to the federal constitutional system (which retains gatekeeper power over exit by requiring its own consent via amendment). Transfers political power from regions seeking exit to the central state and federal minorities who benefit from territorial preservation.
% ABSENT_VOICES: Indigenous treaty holders are structurally excluded from the secession legitimacy conversation — their treaty-based territorial authority predates both federal and provincial frameworks, but this constraint treats secession as a binary between federal and provincial (pro-exit regional majority vs. federal authority) and does not recognize treaty holder claims as independently relevant to territorial legitimacy. Regions that have historically exited or attempted to exit are not in the room; neither are diaspora populations or former federal citizens.
% DISAPPEARANCE_RATIONALE: If the constitutional rule disappeared, regional majorities would face a different political landscape: some regions might exit via referendum; others might negotiate exit on improved terms; the federation itself might dissolve or reorganize. The constraint does not create the underlying desire to exit, but it does create the gatekeeping rule that channels such desires through federal-consent-requiring amendment rather than direct regional choice. A world without the constraint would see different territorial politics, but whether the federation would actually dissolve is contested between those who believe exit desires are fundamental and those who believe the constraint itself holds the union together.
% FOUNDING_PROBLEM: Historical secession crises threatened to dissolve the federation through unilateral exit: regions that faced federal policies they opposed sought to leave, creating an existential threat to the central state. The constitutional rule was established to prevent this — to make exit structurally difficult enough that exit threats could not be deployed as bargaining leverage for policy concessions.
% FOUNDING_PROBLEM_CORROBORATION: The federal authority and constitutional court attest the founding problem is perpetually live: exit sentiment persists in some regions, and the rule is necessary to keep it from materializing. Separatist movements and regional majorities attest the founding problem is substantially solved or transformed (exit desire exists but the rule prevents it only through coercion, not through resolution of underlying grievance — the problem persists but is suppressed, not solved). International observers and legal scholars document that founding crises occurred at federation formation; whether they remain existential depends on counterfactual reasoning that cannot be empirically adjudicated.
narrative_ontology:disappearance_verdict(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).
narrative_ontology:founding_problem_status(secession_legitimacy_boundary__constitutional_impossibility_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(secession_legitimacy_boundary__constitutional_impossibility_reading, 'none', 1).
narrative_ontology:epsilon_provenance(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(secession_legitimacy_boundary__constitutional_impossibility_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, ExtMetricName, E),
    domain_priors:suppression_score(secession_legitimacy_boundary__constitutional_impossibility_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(secession_legitimacy_boundary__constitutional_impossibility_reading),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(secession_legitimacy_boundary__constitutional_impossibility_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(secession_legitimacy_boundary__constitutional_impossibility_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness value (0.62, rising over the interval) reflects that the constraint transfers legitimacy authority from regional democratic majorities to the federal constitutional system, which retains gatekeeper power. The constraint is presented as natural law (constitutional logic, federalism structure) but operates functionally as a political choice protected by institutional power: the federal authority benefits from it, the constitutional court derives authority from enforcing it, and regional exit seekers bear the cost. The suppression requirement (0.71, rising) indicates that the constraint requires active enforcement — constitutional courts must repeatedly rule exit bids unconstitutional, federal authority must maintain the amendment requirement and make clear that unilateral exit will not be recognized, separatist movements must be kept from shifting from political fringe to mainstream legitimacy claim. Theater ratio (0.42, rising) indicates a modest proportion of performative activity: constitutional courts issue rulings largely for precedent and public legitimacy affirmation (the outcome is predetermined by the reading); federal authority makes speeches about constitutional integrity that serve as much to consolidate internal support as to external deterrence. The accessibility_collapse (0.78) is high, reflecting that once the constitutional rule is understood, alternatives are nearly completely closed — exit is not available through referendum, democratic supermajorities, or bargaining unless the constitution is amended (which requires federal consent, structurally unlikely when exit would diminish federal territory). Resistance (0.59) is moderate: separatist movements mount real resistance through mobilization, legal challenges, and political pressure, but the constitutional text and institutional hierarchy limit their effectiveness.
 *
 * PERSPECTIVAL GAP:
 *   From the federal authority seat, the constraint is a genuine mountain — constitutional logic that must be upheld regardless of regional sentiment. From the provincial exit-seeking seat, the constraint is extractive: it transfers their exit decision to federal-controlled amendment process, effectively giving the federal majority a veto over exit they do not believe should have legitimacy. From the constitutional court seat, the constraint is a legitimate application of constitutional text. From the international observer seat, the constraint is one possible reading among many — some federations have unilateral exit or negotiated exit clauses; the question is whether the constitutional text actually requires this reading or whether alternative readings are textually sustainable. The engine will compute different types for each seat from the same structural data: the federal authority may compute as mountain or rope (depending on whether emergence is genuinely natural or has been defended as natural). The exit-seeking seat will compute as snare (trapped, high suppression, payee of transfer). The constitutional court will compute as beneficiary of the constraint (derives institutional authority from enforcing it).
 *
 * DIRECTIONALITY LOGIC:
 *   Federal authority benefits from the constraint (controls the amendment gate, maintains territorial integrity, derives legitimacy from upholding constitutional supremacy) — d approaches 0 (full beneficiary). Provincial majority seeking exit pays through categorical delegitimization and exit closure — d approaches 1 (full target). Federal minorities benefit from territorial preservation that would not survive regional exit — d moderately low (secondary beneficiary). Constitutional court derives institutional power from enforcing the constraint — d low (beneficiary, though less directly than federal authority). Separatist movements and treaty holders face suppression and exclusion — d moderately high to high (targets). The directionality structure is asymmetric by institutional power: powerful institutional seats (federal authority, constitutional court) sit at the beneficiary end; moderate and lower-power seats (regional majorities, separatist movements, indigenous treaty holders) sit at the target end. This asymmetry is not an accident — the constraint is enforced by the powerful institutional seats who benefit from it. No override is necessary; the structural derivation from beneficiary/victim declarations and exit options captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is territorial integrity through constitutional indivisibility — preventing secession by making it unconstitutional. The mandate persists unchanged: territorial boundaries remain intact, no region has successfully exited, and the constitutional amendment bar remains (formally) intact. However, the mounting extractiveness (0.48 → 0.62 over the interval) and rising theater ratio (0.25 → 0.42) suggest that the constraint's function is shifting. Early in the interval, the constraint operated to deter exit attempts through legal clarity. Over time, as separatist sentiment persists despite the constitutional prohibition, enforcement activity increases (suppression_requirement rises from 0.58 to 0.71) and more of that activity becomes performative — constitutional courts issue reinforcing rulings, federal authority makes speeches about indivisibility, the legal rule is restated rather than generating new deterrence. This is consistent with mandatrophy: the founding problem (exit threats at federation's formation) no longer exists in the same acute form (exit movements remain but are marginalized, not existential), yet the constraint persists through institutional inertia and institutional benefit (courts derive authority from enforcing it, federal authority benefits from territorial preservation). The rising theater ratio is the diagnostic signal: enforcement activity is increasingly about theatrical reaffirmation rather than functional deterrence. The constraint is not dead (territorial preservation is still real), but the ratio of performance to function is increasing, consistent with a piton or false-summit trajectory.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_boundary,
    'Is the constitutional impossibility of secession a natural feature of federalism itself (inherent to any coherent federal structure), or a constructed political choice that this particular constitution makes?',
    'Comparative constitutional analysis: do other stable federations permit unilateral secession or constitutional exit paths? Are there federations without explicit anti-secession clauses that still treat exit as impermissible? Does the foundational logic of federalism require territorial indivisibility?',
    'If natural/inherent: the constraint is genuinely a mountain and the beneficiary class (federal authority) is incidental. If constructed: the constraint is a false summit — a political choice protected by constitutional text and institutional power, not by immutable logic. The presence of beneficiaries (federal_authority, constitutional_court) suggests constructed status; the high accessibility_collapse suggests naturalness claimed but possibly fabricated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_boundary, conceptual, 'Whether secession impossibility follows from the nature of federalism or from this constitution''s design choice.').

omega_variable(
    alternative_reading_foreclosure,
    'Does the constitutional text FORECLOSE the popular_sovereignty reading and the grievance_threshold reading, or merely override them as matter of legal hierarchy?',
    'Textual and doctrinal analysis: can a court interpret the same constitutional document to permit exit in cases of severe injustice or overwhelming democratic mandate? Or does the text logically exclude those interpretations? Is the foreclosure textual or jurisdictional (the court system is structured to read the text in one way)?',
    'If textual foreclosure: this reading and the sibling readings are genuinely mutually exclusive — one court framework cannot hold both. If jurisdictional: a different interpretive community (e.g., a different constitutional court, a revolutionary tribunal) could adopt the sibling reading from the same text. This affects whether reading_relations should include forecloses (true foreclosure) or coexists_with (institutional segregation).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_reading_foreclosure, conceptual, 'Whether sibling readings are logically incompatible with this reading or merely politically defeated.').

omega_variable(
    identity_lock_mechanism,
    'Is the identity_locked exit option for provincial majorities structural (legal barriers, constitutional requirement for federal consent) or internalized (the population accepts federal legitimacy and treats unilateral exit as genuinely illegitimate, not merely illegal)?',
    'Post-constraint-removal measurement: if the constitutional rule were suspended or amended to permit unilateral exit, would regional exit movements immediately surge, or would cultural/political identity fusion with the federal state prevent mobilization? Do separatist movements exist but remain marginal because the rule is internalized, or do they remain marginal because the rule is genuinely effective?',
    'If structural: the suppression value (0.71) understates the true suppressive force — the identity lock adds internalized suppression on top. If internalized: the constraint is self-sustaining and less fragile than the raw suppression value suggests. This affects whether the constraint would collapse if enforcement capacity weakened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Whether exit closure is externally imposed or internalized as identity.').

omega_variable(
    this_reading_kernel_settlement,
    'Does this reading claim to settle the kernel authoritatively, or does it occupy one institutional seat in an ongoing contested kernel?',
    'Institutional and political analysis: does the federal authority treat alternative readings as legitimate but inferior interpretations (coexistent readings), or as categorically illegitimate (foreclosed readings)? Are sibling readings given institutional recognition (courts, legislatures entertaining them seriously) or actively suppressed?',
    'If this reading claims kernel-settlement: the constraint is more likely a false summit (beneficiaries defending constructed hierarchy as natural law). If this reading occupies one seat in a multi-seat contest: the constraint is tangled_rope or snare (active enforcement needed to suppress sibling readings). The measured metrics (0.62 extractiveness, 0.71 suppression) and rising theater ratio (0.25 → 0.42) are consistent with active enforcement against competing readings.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(this_reading_kernel_settlement, conceptual, 'Whether this reading settles the kernel or competes with sibling readings for institutional authority.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(secession_legitimacy_boundary__constitutional_impossibility_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sece_tr_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(sece_tr_t0, observed).
narrative_ontology:measurement(sece_tr_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 10, 0.3).
narrative_ontology:measurement_basis(sece_tr_t10, observed).
narrative_ontology:measurement(sece_tr_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 20, 0.36).
narrative_ontology:measurement_basis(sece_tr_t20, observed).
narrative_ontology:measurement(sece_tr_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 30, 0.4).
narrative_ontology:measurement_basis(sece_tr_t30, observed).
narrative_ontology:measurement(sece_tr_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 40, 0.41).
narrative_ontology:measurement_basis(sece_tr_t40, observed).
narrative_ontology:measurement(sece_tr_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, theater_ratio, 50, 0.42).
narrative_ontology:measurement_basis(sece_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(sece_be_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(sece_be_t0, observed).
narrative_ontology:measurement(sece_be_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 10, 0.52).
narrative_ontology:measurement_basis(sece_be_t10, observed).
narrative_ontology:measurement(sece_be_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(sece_be_t20, observed).
narrative_ontology:measurement(sece_be_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement_basis(sece_be_t30, observed).
narrative_ontology:measurement(sece_be_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement_basis(sece_be_t40, observed).
narrative_ontology:measurement(sece_be_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(sece_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(sece_su_t0, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(sece_su_t0, observed).
narrative_ontology:measurement(sece_su_t10, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(sece_su_t10, observed).
narrative_ontology:measurement(sece_su_t20, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 20, 0.67).
narrative_ontology:measurement_basis(sece_su_t20, observed).
narrative_ontology:measurement(sece_su_t30, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 30, 0.7).
narrative_ontology:measurement_basis(sece_su_t30, observed).
narrative_ontology:measurement(sece_su_t40, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(sece_su_t40, observed).
narrative_ontology:measurement(sece_su_t50, secession_legitimacy_boundary__constitutional_impossibility_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(sece_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(secession_legitimacy_boundary__constitutional_impossibility_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(secession_legitimacy_boundary__constitutional_impossibility_reading, 0.12).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__popular_sovereignty_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__grievance_threshold_reading).
narrative_ontology:affects_constraint(secession_legitimacy_boundary__constitutional_impossibility_reading, secession_legitimacy_boundary__treaty_primacy_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the contested kernel 'secession_legitimacy_boundary'. Four separate constraint stories instantiate four distinct readings: constitutional_impossibility (this file), popular_sovereignty (provincial referenda supreme), grievance_threshold (injustice threshold overrides), and treaty_primacy (indigenous authority primary). Each reading has its own ε (referent: the standing territorial arrangement under contest, assessed through each reading's epistemic lens). The readings are linked by affects_constraints edges, indicating that acceptance of one reading structurally forecloses or delegitimizes others. The constraint family exhibits asymmetric institutional power: the constitutional_impossibility reading is institutionally dominant (enforced by central courts), making the other readings structurally marginal even where they have popular support.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
