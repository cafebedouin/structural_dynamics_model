% ============================================================================
% CONSTRAINT STORY: sex_gender_category__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__hybrid_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: sex_gender_category__hybrid_reading
 *   human_readable: Sex/Gender Category Membership via Medical Gatekeeping (Hybrid Reading)
 *   domain: social/legal/medical
 *
 * SUMMARY:
 *   This story instantiates ONE READING of a contested kernel: sex/gender
 *   category membership. The hybrid reading asserts that category membership
 *   is determined by a combination of biological sex (as a material fact,
 *   typically reproductive anatomy and chromosomes at birth) AND demonstrated
 *   social/medical transition (hormone therapy, often surgical
 *   reconstruction, documented by medical professionals). This reading is
 *   distinct from two sibling readings: the biology reading (membership
 *   determined by biology alone, identity irrelevant) and the identity
 *   reading (membership determined by subjective identity alone, biology
 *   irrelevant). The hybrid reading's warrant is administrative legitimacy—it
 *   offers institutions a way to recognize trans individuals while
 *   maintaining a category boundary that requires demonstrable medical
 *   transition rather than identity claim alone. Under this reading, trans
 *   women who have medically transitioned are conditionally included in the
 *   woman category; non-transitioning trans individuals are excluded. The
 *   model concentrates authority in medical institutions, creating high
 *   gatekeeping costs and producing a victim set that includes both those
 *   excluded entirely and those conditionally included but paying for access
 *   through medical surveillance and bodily modification.
 *
 * KEY AGENTS:
 *   - Medical gatekeepers (physicians, psychiatrists, endocrinologists, credentialing bodies): institutional actors who control access to transition and define adequacy criteria.
 *   - Trans women post-transition: conditionally beneficiary/payer — gain category recognition at the cost of medical surveillance and bodily modification.
 *   - Non-transitioning trans individuals: victims — permanently excluded, no legitimacy path available.
 *   - Cis women and cis men: beneficiaries — category boundary maintained through gatekeeping requirement.
 *   - Medical transition seekers: payers — absorb delays, costs, medical control, and protocol surveillance.
 *   - Legal/institutional administrators: agenda-setters who co-administer with medical gatekeepers by embedding transition criteria into law.
 *   - Trans advocacy organizations: observers split across the kernel contest — some supporting, others opposing this reading.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__hybrid_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__hybrid_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(sex_gender_category__hybrid_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__hybrid_reading, "Sex/Gender Category Membership via Medical Gatekeeping (Hybrid Reading)").
narrative_ontology:topic_domain(sex_gender_category__hybrid_reading, "social/legal/medical").

domain_priors:requires_active_enforcement(sex_gender_category__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__hybrid_reading, 'cae193f9-e023-4e73-8ac3-c1e559a813e3').
narrative_ontology:cs_kernel_codification('cae193f9-e023-4e73-8ac3-c1e559a813e3', distributed).
narrative_ontology:cs_authority_grounding('cae193f9-e023-4e73-8ac3-c1e559a813e3', extraction).
narrative_ontology:cs_interpretation_layer_present('cae193f9-e023-4e73-8ac3-c1e559a813e3').
narrative_ontology:cs_reading_relation('cae193f9-e023-4e73-8ac3-c1e559a813e3', sex_gender_category__biology_reading, coexists_with).
narrative_ontology:cs_reading_relation('cae193f9-e023-4e73-8ac3-c1e559a813e3', sex_gender_category__identity_reading, coexists_with).
narrative_ontology:cs_axiom('cae193f9-e023-4e73-8ac3-c1e559a813e3', foundational, reproductive_biology_materially_relevant).
narrative_ontology:cs_axiom_status(reproductive_biology_materially_relevant, holdable).
narrative_ontology:cs_axiom_grounding('cae193f9-e023-4e73-8ac3-c1e559a813e3', reproductive_biology_materially_relevant, empirically_contingent).
narrative_ontology:cs_axiom('cae193f9-e023-4e73-8ac3-c1e559a813e3', foundational, medical_transition_legitimates_category_shift).
narrative_ontology:cs_axiom_status(medical_transition_legitimates_category_shift, holdable).
narrative_ontology:cs_axiom_grounding('cae193f9-e023-4e73-8ac3-c1e559a813e3', medical_transition_legitimates_category_shift, instrumental).
narrative_ontology:cs_reference_frame('cae193f9-e023-4e73-8ac3-c1e559a813e3', sex_category_via_birth_assignment_and_medical_credentialing).
narrative_ontology:cs_drift_state('cae193f9-e023-4e73-8ac3-c1e559a813e3', contemporary_gatekeeping_expansion, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('cae193f9-e023-4e73-8ac3-c1e559a813e3', '2026-06-15T14:32:18Z').
narrative_ontology:cs_kernel_id(sex_gender_category__hybrid_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, medical_transition_gatekeepers).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cis_women_category_holders).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, medical_transition_seekers).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_men_assigned_female).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, trans_women_post_transition).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, cis_men_category_holders).
narrative_ontology:constraint_beneficiary(sex_gender_category__hybrid_reading, legal_institutional_administrators).
narrative_ontology:constraint_victim(sex_gender_category__hybrid_reading, trans_women_post_transition).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, biological_sex_materially_meaningful).
narrative_ontology:constraint_vindicates(sex_gender_category__hybrid_reading, medical_transition_legitimates_category_shift).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Physicians, psychiatrists, endocrinologists, and credentialing bodies that set transition protocols and control access to medical services. They author what 'successful' transition means, hold gatekeeping authority, and maintain institutional backing through licensing and malpractice law. Their interest in the hybrid reading is the preservation of medical authority over gender recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_transition_gatekeepers, agenda_setter,
    institutional, generational, arbitrage, national).

% People assigned female at birth with uncontested woman identity. Category membership is stable and unquestioned. They benefit from the gatekeeping model because it maintains a boundary that requires medical transition before category entry from the assigned-male side, preserving category distinctiveness.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cis_women_category_holders, beneficiary,
    organized, generational, mobile, national).

% Trans women who have undergone or are undergoing medical transition. They gain conditional inclusion in the woman category but pay through medical costs, surveillance, bodily modification, and the conditional/revocable nature of category membership. Their exit option is constrained: declining transition means exclusion; identity claim alone does not suffice under this reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_women_post_transition, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, trans_women_post_transition, payer).

% Trans individuals who do not pursue medical transition for reasons including cost, medical contraindication, dysphoria profile, religious conviction, discomfort with medicalization, or lack of access. They bear permanent category exclusion, institutional deadnaming, exclusion from facilities, and social stigma. Trapped because they cannot access category membership through any available route that fits their circumstances.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, non_transitioning_trans_individuals, payer,
    powerless, biographical, trapped, national).

% Trans men assigned female at birth. They face the non-reciprocal burden that their reproductive biology (assigned female) is harder to neutralize medically than assigned-male biology is for trans women. Identity-locked because accepting the hybrid reading's validity requires accepting institutional gatekeeping as just, which contradicts their experienced injustice; rejecting it cuts them from medical support they may need.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_men_assigned_female, payer,
    powerless, biographical, identity_locked, national).

% Individuals seeking medical transition (trans women, trans men, non-binary people) who accept the medical gatekeeping model as legitimate but encounter delays, cost barriers, provider discretion, mandatory psychiatric evaluation, and protocol-imposed timelines. They pay through waiting, surveillance, bodily modification, and deference to medical judgment. Constrained exit: declining transition forecloses the only recognized path to category shift; seeking transition outside medical systems forecloses legal/institutional recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, medical_transition_seekers, payer,
    moderate, biographical, constrained, national).

% People assigned male at birth with uncontested man identity. Their category membership is uncontested. They benefit from the gatekeeping model because entry to the man category from outside requires medical transition away from assigned-female biology, which is medically more involved, maintaining category boundary clarity.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, cis_men_category_holders, beneficiary,
    organized, generational, mobile, national).

% Courts, legislatures, administrative agencies, and government bodies that implement sex/gender category policy through law. They benefit from the medical gatekeeping model because it outsources categorization to medical experts, reduces political burden, provides objective-looking criteria, and offers administrative clarity. They co-administer with medical gatekeepers by embedding transition criteria into legal recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, legal_institutional_administrators, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__hybrid_reading, legal_institutional_administrators, beneficiary).

% Individuals (non-binary people, non-transitioning gender-nonconforming people, some trans people whose identity does not align with binary categories) who would claim category membership or category-independent recognition on the basis of identity or social role alone. Excluded from participation in gatekeeping rule-setting; their voice would fundamentally alter the model.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, excluded_identity_claimants, excluded,
    powerless, biographical, trapped, national).

% Organizations advocating for trans rights. They observe and contest from multiple positions (some supporting conditional medical-transition-based recognition, others opposing gatekeeping and demanding identity-based recognition). Analytical observers to this reading, though advocates in the larger kernel contest.
narrative_ontology:constraint_stakeholder(sex_gender_category__hybrid_reading, trans_advocacy_organizations, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__hybrid_reading, medical_transition_gatekeepers).
narrative_ontology:fixing_cost_class(sex_gender_category__hybrid_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a criterion for category membership that combines biological markers with medical transition as a demonstrable threshold, enabling institutions to make consistent, legible, and defensible category assignments without requiring metaphysical judgment about internal identity states. Solves the administrative problem: how do organizations (schools, sports bodies, medical records systems, law enforcement, census authorities) classify individuals when self-identification is unverifiable and reproductive biology alone is contested as category basis?
% TRANSFER_FUNCTION: Transfers authority over category membership from individuals (identity claimants) to medical institutions. Medical gatekeepers collect the power to define what counts as 'adequate' transition and the gatekeeping authority; cis-category holders collect the benefit of category boundary maintenance under a rule that requires objective demonstration of transition rather than accepting identity claim alone. Non-transitioning trans individuals and medical-transition seekers transfer their category-membership destiny to medical institutions, their bodily autonomy to medical protocols, and their timeline to medical timescales and provider discretion.
% ABSENT_VOICES: Non-transitioning trans individuals are largely excluded from participation in rule-setting; they would argue that medical transition should not be a requirement for category recognition and that the gatekeeping model perpetuates a false conflation of medical transition with gender legitimacy. Some non-binary people and gender-nonconforming people are excluded; they would argue for category-independent gender recognition or for a non-binary category itself. Trans advocacy organizations span the divide — some support this reading, others oppose it entirely on the grounds that it medicalizes gender identity.
% DISAPPEARANCE_RATIONALE: If medical gatekeeping were removed, category membership would rearrange immediately: legal systems would face urgent pressure to adopt an alternative criterion (either pure biology or pure identity or explicitly plural categories). Medical institutions would lose gatekeeping authority and could not control who accesses medical transition or whose transition 'counts.' Cis-category holders would face an altered boundary (identity-based entry without transition requirement under an identity reading, or contested multi-category presence). Trans individuals would reorganize around either identity-based recognition or non-medical community recognition. The institutional landscape would reorganize rapidly.
% FOUNDING_PROBLEM: Early medical/legal engagement with trans identity faced a coordination problem: institutions needed a way to classify individuals for facility access, legal documents, and records when self-identification alone was unverifiable and could not be administratively audited; biological sex seemed objective but was contested as the basis for gender; medicine offered transition as a demonstrable, verifiable, and institutionally documentable marker that could bridge the legitimacy gap and provide institutional cover for category shift.
% FOUNDING_PROBLEM_CORROBORATION: Medical institutions and legal authorities attest the founding problem is still live, citing administrative necessity and the need for objective criteria. Trans medical transition seekers and many cis-category holders attest that the problem is real and the medical gatekeeping solution is justified. Opposing parties — non-transitioning trans individuals and identity-based recognition advocates — attest that the 'founding problem' was constructed to serve gatekeeping interests, that medical institutions invented the necessity for verification, and that the problem would dissolve under identity-based recognition. Independent scholars and human-rights monitors document that the gatekeeping apparatus has outlived its justification and has become a tool for subordination rather than coordination.
narrative_ontology:disappearance_verdict(sex_gender_category__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__hybrid_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the constraint extracts from those it governs, relative to the coordination function it provides. At interval start (t=0), extractiveness is moderate (0.48) because the hybrid model does solve a real administrative problem — institutions need criteria. Over the interval, extractiveness rises to 0.68 as the gatekeeping apparatus becomes an end in itself rather than a means to coordination: medical transitions lengthen (more gatekeeping delay), criteria tighten (medical gatekeepers set higher bars for 'adequacy'), and legal systems embed medical criteria more rigidly. Suppression is high (0.72 at end) because the constraint's persistence requires actively suppressing alternative recognition pathways (identity-based, non-binary categories, non-medical community recognition) and surveillance of transition adequacy. The suppression is structural: legal systems must refuse identity-only claims, medical systems must deny transition access to those deemed 'inadequate,' and institutions must align with medical authority rather than individual choice. Theater ratio (0.41) indicates that a substantial share of gatekeeping activity is performative — the medical review of transition adequacy is presented as clinical assessment but increasingly operates as legitimacy judgment (is this transition 'real enough'?), which is not a medical question. Accessibility collapse is moderate (0.58) because alternatives do technically exist — non-medical social transition, legal recognition in some jurisdictions without medical transition, non-binary categories in some contexts — but are institutionally hostile and unrecognized. Resistance is high (0.69) because the constraint meets active resistance from non-transitioning trans individuals, non-binary advocates, and trans-rights organizations that reject medical gatekeeping as unjust.
 *
 * PERSPECTIVAL GAP:
 *   The four stakeholder seats experience this constraint radically differently. From the medical gatekeeper seat: the constraint is genuine coordination solving administrative necessity. From the cis-woman seat: the constraint is a boundary-maintenance mechanism protecting category integrity. From the trans-woman-post-transition seat: the constraint is mixed — genuine gains from inclusion paired with extraction costs (medical surveillance, bodily modification, conditionality). From the non-transitioning-trans-individual seat: the constraint is pure exclusion with no exit; it is a snare, not a rope. From the legal-administrator seat: the constraint is a way to outsource the difficult category decision to medicine. The engine computes per-seat classifications from power level, exit options, and directionality; these seats carry radically different power atoms (institutional vs. powerless), exit options (arbitrage vs. trapped), and directionality (near-beneficiary vs. near-target), so the computed types will diverge sharply. The claimed type (tangled_rope) is the reading-level assessment: the constraint has a genuine coordination function (administrative legibility, category boundary maintenance) paired with asymmetric extraction (medical gatekeeping, conditional access, surveillance). The within-reading claim that it is tangled_rope is not invalidated by the fact that non-transitioning individuals compute it as snare — their divergence is precisely the seat-level classification the engine measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Medical gatekeepers are near d=0.0 (full beneficiary): they collect gatekeeping authority, control resource access, shape institutional legitimacy, and face low exit costs (they can simply stop administering the system if it were dismantled; they suffer no ongoing extraction). Cis-category holders are near d=0.0 to 0.3 (beneficiary to symmetric): they collect the benefit of a maintained boundary without paying medical costs themselves, though they do carry a diffuse cost in increased institutional complexity and the burden of boundary-maintenance surveillance. Trans-women-post-transition are near d=0.5 to 0.7 (symmetric to target): they gain category recognition (beneficiary direction) but pay through medical costs, surveillance, and conditionality (target direction); the balance depends on how much they value category inclusion vs. how much they resent the costs. Non-transitioning-trans-individuals are near d=1.0 (full target): they bear pure extraction (permanent exclusion, no exit) with no coordination benefit. Medical-transition-seekers are near d=0.6 to 0.8 (target to full target): they pay through delays, costs, surveillance, and deference to medical judgment. Trans-men-assigned-female are near d=0.7 (target end, toward full target): they face the non-reciprocal burden (their biology is harder to neutralize), identity-locked exit (accepting the reading requires accepting the injustice; rejecting it cuts them from medical support), and permanent exclusion without transition. The directionality pattern supports the tangled_rope claim: the constraint has coordination (boundary maintenance) paired with asymmetric extraction (medical gatekeeping authority concentrated, costs borne by transition-affected individuals).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is 'institutions need objective criteria for category assignment.' Under the hybrid reading, medical transition is the demonstrated marker that provides that objectivity. However, the corpus's mandatrophy clock (founding_problem_status x disappearance_verdict) shows: founding_problem_status is contested (not universally agreed to be live); disappearance_verdict is world_rearranges (the arrangement would precipitate institutional reorganization if removed). The contradiction flags mandatrophy: the founding problem is not universally acknowledged, yet its solution is deeply embedded (world would rearrange without it). This signals that the gatekeeping apparatus has become an end in itself — it persists not because the founding problem is live, but because institutions are now dependent on it and removing it would be disruptive. The theater ratio (rising from 0.25 to 0.41) supports this diagnosis: an increasing share of gatekeeping activity is about maintaining the gatekeeper role rather than solving the administrative problem. The medical review of transition adequacy should be straightforward (has hormone therapy begun? has surgical reconstruction occurred?); its complexity and the stringency of criteria suggest that gatekeeping has become a legitimacy judgment, which is extraction, not coordination. Mandatrophy is declared: the hybrid reading's warrant (administrative necessity) has outlived its function; the gatekeeping apparatus persists through institutional inertia and gatekeeper interest, not through the problem it was built to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    medical_necessity_vs_gatekeeping_motivation,
    'Is the medical gatekeeping requirement genuinely necessary for administrative legibility, or has it become a mechanism for medical institutions to control gender-related medical care and for cis-category holders to suppress non-medical gender recognition?',
    'Comparative-institutional analysis: do jurisdictions that recognize gender identity without medical transition requirement experience worse administrative outcomes (more fraud, administrative confusion, institutional dysfunction) than jurisdictions with medical gatekeeping? Natural experiments from jurisdictions that have relaxed transition requirements provide data.',
    'If administrative necessity is demonstrated, the medical gatekeeping model is more rope (genuine coordination cost) than snare (extraction covering for gatekeeping). If administrative outcomes are equivalent or better without gatekeeping, the extraction reading dominates and the constraint is more snare than rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_necessity_vs_gatekeeping_motivation, empirical, 'Whether medical gatekeeping is administratively necessary or is extraction in disguise.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the suppression of non-medical gender recognition and non-transitioning-trans-individual voice primarily structural (legal barriers, medical licensing rules, institutional policies) or internalized (trans individuals internalize the medical legitimacy narrative, non-transitioning individuals accept their exclusion as justified)?',
    'Post-gatekeeper-removal trajectory: if suppression persists after legal/medical barriers are formally removed, the suppression is at least partly internalized. If suppression collapses immediately when barriers are removed, it is primarily structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, and exit is identity-locked for those who have internalized medical gatekeeping as legitimate. If structural, removal of barriers would open alternatives rapidly.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized suppression in the medical gatekeeping model.').

omega_variable(
    reading_foreclosure_biology_vs_hybrid,
    'Does the hybrid reading''s acceptance of medical transition as category-shifting foreclosure the biology reading''s core claim, or do the two readings coexist as competing institutional paradigms?',
    'Institutional analysis: can a single institutional framework hold both readings (e.g., recognize trans individuals post-transition while also recognizing biology as materially relevant in some contexts)? Or does adoption of the hybrid reading necessarily displace biology-only framing?',
    'If the hybrid reading forecloses biology-only framing (logically rules it out), the reading_relations should be forecloses; if they coexist as different institutions hold different readings, it should be coexists_with. This determination affects the structural_relationship computation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_foreclosure_biology_vs_hybrid, conceptual, 'Whether the hybrid reading logically forecloses the biology reading or whether they coexist.').

omega_variable(
    trans_men_asymmetric_burden,
    'Is the non-reciprocal burden on trans men (their reproductive biology is harder to neutralize medically than assigned-female anatomy is for trans women) a feature of the hybrid reading''s structure or an artifact of current medical technology?',
    'Medical development: if medical science develops reversible, non-surgical ways to neutralize reproductive-system markers, does the asymmetry disappear? Or is the asymmetry baked into the reading''s logic (that reproductive biology is a salient category marker)?',
    'If the asymmetry is technological, the reading''s burden distribution could be equalized by medical development, reducing the victim set asymmetry. If structural, the reading inherently produces asymmetric burden on trans men, which may be a sign that the reading is unsustainable or unjust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trans_men_asymmetric_burden, empirical, 'Whether trans-men asymmetry is technological or structural.').

omega_variable(
    medical_institutionalization_critique,
    'To what extent does the hybrid reading conflate gender identity with medical transition not because transition is necessary for legitimacy, but because medical institutions can measure and control transition while they cannot measure or control identity?',
    'Institutional history and power analysis: trace how medical institutions became the authority on gender recognition. Did medical authority emerge from medical necessity or from institutional power-seeking and the absence of alternatives? What role did the lack of trans advocacy power play in allowing medicine to monopolize legitimacy?',
    'If medical institutionalization was power-driven rather than necessity-driven, the gatekeeping apparatus is more snare (extraction using institutional control) than rope (genuine coordination). The reading itself may be a false summit — presented as necessary coordination but actually serving extractive interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medical_institutionalization_critique, conceptual, 'Whether medical gatekeeping is necessary or an artifact of institutional power asymmetry.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__hybrid_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__hybrid_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(sex__tr_t7, sex_gender_category__hybrid_reading, theater_ratio, 7, 0.31).
narrative_ontology:measurement(sex__tr_t14, sex_gender_category__hybrid_reading, theater_ratio, 14, 0.36).
narrative_ontology:measurement(sex__tr_t21, sex_gender_category__hybrid_reading, theater_ratio, 21, 0.4).
narrative_ontology:measurement(sex__tr_t28, sex_gender_category__hybrid_reading, theater_ratio, 28, 0.41).
narrative_ontology:measurement(sex__tr_t35, sex_gender_category__hybrid_reading, theater_ratio, 35, 0.41).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__hybrid_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sex__be_t7, sex_gender_category__hybrid_reading, base_extractiveness, 7, 0.54).
narrative_ontology:measurement(sex__be_t14, sex_gender_category__hybrid_reading, base_extractiveness, 14, 0.61).
narrative_ontology:measurement(sex__be_t21, sex_gender_category__hybrid_reading, base_extractiveness, 21, 0.66).
narrative_ontology:measurement(sex__be_t28, sex_gender_category__hybrid_reading, base_extractiveness, 28, 0.67).
narrative_ontology:measurement(sex__be_t35, sex_gender_category__hybrid_reading, base_extractiveness, 35, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__hybrid_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sex__su_t7, sex_gender_category__hybrid_reading, suppression_requirement, 7, 0.62).
narrative_ontology:measurement(sex__su_t14, sex_gender_category__hybrid_reading, suppression_requirement, 14, 0.68).
narrative_ontology:measurement(sex__su_t21, sex_gender_category__hybrid_reading, suppression_requirement, 21, 0.71).
narrative_ontology:measurement(sex__su_t28, sex_gender_category__hybrid_reading, suppression_requirement, 28, 0.72).
narrative_ontology:measurement(sex__su_t35, sex_gender_category__hybrid_reading, suppression_requirement, 35, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__hybrid_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__hybrid_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__hybrid_reading, sex_gender_category__identity_reading).

% DUAL FORMULATION NOTE:
% The sex/gender category kernel decomposes into three structurally distinct constraints, one for each reading. The biology_reading assigns membership by immutable reproductive biology alone (Mountain candidate). The identity_reading assigns membership by subjective identity alone (Rope candidate, lower extraction). The hybrid_reading (THIS FILE) assigns membership by biology + demonstrated medical transition (Tangled Rope, higher extraction due to gatekeeping). The three readings are not measurement-basis choices on a single constraint — they are three logically incompatible claims about what determines legitimacy, and they have different beneficiary/victim structures, different ε values, and different classifications. Each story carries its own structural data; the network links them to document the kernel contest.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__hybrid_reading, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
