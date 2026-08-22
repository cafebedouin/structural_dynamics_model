% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__biological_sex_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__biological_sex_reading, []).

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
 *   constraint_id: gendered_category_membership__biological_sex_reading
 *   human_readable: Gendered Category Membership — Biological Sex Reading
 *   domain: social_ontology/political_philosophy/bioethics
 *
 * SUMMARY:
 *   This constraint instantiates the biological-sex reading of gendered
 *   category membership: 'woman' is defined by immutable biological markers
 *   present at birth (chromosomes, reproductive anatomy, reproductive
 *   capacity). Under this reading, trans women are excluded from the
 *   category; sex-segregated spaces preserve a binary biological boundary;
 *   institutional gatekeepers (medical, legal, administrative) control
 *   category certification. The reading frames this as necessary for feminist
 *   coherence and space integrity. Identity-reading advocates contest it as
 *   exclusionary and empirically unfounded (sex markers are continuous, not
 *   binary; identity is psychologically constitutive). This is one reading of
 *   a contested kernel — the constraint is authored as coherent and
 *   internally consistent, not as adjudicated between readings. The engine
 *   measures whether the structure produces the extractive pattern this
 *   reading's beneficiary and victim declarations entail.
 *
 * KEY AGENTS:
 *   - cis_women_as_category_boundary_defenders: primary beneficiary and partial agenda-setter (organized power, identity-locked exit, generational horizon)
 *   - trans_women: primary victims (powerless, identity-locked, biographical horizon, high exclusion cost)
 *   - sex_segregated_space_designers: secondary victims/payers (moderate power, constrained exit, operational cost of enforcement)
 *   - feminist_theorists_sex_essentialism_school: beneficiary via theoretical vindication (powerful, organized, gatekeeping authority)
 *   - medical_gatekeepers and legal_systems: agenda-setters (institutional power, gatekeeping authority over category certification)
 *   - identity_cohesion_advocates: observer/challenger (organized, analytical, competing discourse production)
 *   - intersex_and_ambiguous_sex_individuals: excluded (powerless, trapped, no coherent category place under binary criterion)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, 0.78).
domain_priors:suppression_score(gendered_category_membership__biological_sex_reading, 0.71).
domain_priors:theater_ratio(gendered_category_membership__biological_sex_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(gendered_category_membership__biological_sex_reading, resistance, 0.73).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__biological_sex_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__biological_sex_reading, "Gendered Category Membership — Biological Sex Reading").
narrative_ontology:topic_domain(gendered_category_membership__biological_sex_reading, "social_ontology/political_philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__biological_sex_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__biological_sex_reading, 'c6452e85-8924-49f9-8ec4-af7b918e4f3f').
narrative_ontology:cs_kernel_codification('c6452e85-8924-49f9-8ec4-af7b918e4f3f', formalized).
narrative_ontology:cs_authority_grounding('c6452e85-8924-49f9-8ec4-af7b918e4f3f', extraction).
narrative_ontology:cs_interpretation_layer_present('c6452e85-8924-49f9-8ec4-af7b918e4f3f').
narrative_ontology:cs_reading_relation('c6452e85-8924-49f9-8ec4-af7b918e4f3f', gendered_category_membership__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('c6452e85-8924-49f9-8ec4-af7b918e4f3f', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('c6452e85-8924-49f9-8ec4-af7b918e4f3f', foundational, biological_sex_immutable_at_birth).
narrative_ontology:cs_axiom_status(biological_sex_immutable_at_birth, holdable).
narrative_ontology:cs_axiom_grounding('c6452e85-8924-49f9-8ec4-af7b918e4f3f', biological_sex_immutable_at_birth, empirically_contingent).
narrative_ontology:cs_axiom('c6452e85-8924-49f9-8ec4-af7b918e4f3f', foundational, sex_category_determinative_of_woman_status).
narrative_ontology:cs_axiom_status(sex_category_determinative_of_woman_status, holdable).
narrative_ontology:cs_axiom_grounding('c6452e85-8924-49f9-8ec4-af7b918e4f3f', sex_category_determinative_of_woman_status, deontological).
narrative_ontology:cs_reference_frame('c6452e85-8924-49f9-8ec4-af7b918e4f3f', binary_biological_sex_determination).
narrative_ontology:cs_drift_state('c6452e85-8924-49f9-8ec4-af7b918e4f3f', contemporary_post_identity_recognition_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('c6452e85-8924-49f9-8ec4-af7b918e4f3f', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__biological_sex_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, cis_women_as_category_boundary_defenders).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, trans_women).
narrative_ontology:constraint_victim(gendered_category_membership__biological_sex_reading, sex_segregated_space_designers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__biological_sex_reading, feminist_theorists_sex_essentialism_school).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintain category integrity by insisting on immutable biological markers as the criterion for 'woman.' They argue this criterion preserves the coherence of feminist knowledge claims about sex-based oppression and protects the material stakes of sex segregation (bathrooms, shelters, sports, prisons). Their primary action is defending the boundary through institutional rule-making, legal litigation, and public assertion that biological sex is immutable and category-determinative. They collect symbolic authority and institutional control over who is admitted to 'woman' spaces and knowledge.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women_as_category_boundary_defenders, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__biological_sex_reading, cis_women_as_category_boundary_defenders, agenda_setter).

% Are structurally excluded from the 'woman' category under this reading's criterion. They bear the cost of category exclusion: denial of legal recognition, exclusion from sex-segregated spaces (bathrooms, shelters, prisons, sports leagues), social erasure, and the psychological cost of living under a category assignment that contradicts their self-understanding. Exit is unavailable because identity is, by this reading's logic, immutable at the biological level and self-identification does not override it. Their powerlessness is intensified by the medical gatekeeping and legal regimes that make biological-marker changing technically difficult or legally impossible.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, trans_women, payer,
    powerless, biographical, identity_locked, global).

% Must enforce the biological-sex boundary to maintain the coherence of sex-segregated institutions (bathrooms, shelters, prisons, sports bodies, intimate-care contexts). They face operational costs: verification systems for biological sex, legal liability under conflicting regimes (some jurisdictions accept self-identification, others mandate biological markers), staff training disputes, and public contention over enforcement. They can modify enforcement policy (and bear institutional reputational costs) or maintain strict boundary enforcement (and face legal challenges). They cannot simply abolish the spaces without losing the segregation rationale.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, sex_segregated_space_designers, payer,
    moderate, biographical, constrained, national).

% Vindicate their theoretical framework by maintaining that sex (as biological category) is the irreducible grounding of women's oppression and that category membership must track immutable biological markers to preserve feminist analysis. They produce scholarly and public discourse supporting boundary enforcement and are cited as authoritative in legal and policy disputes. Transition to gender-identity readings would require theoretical reconstruction of their work, which they resist.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, feminist_theorists_sex_essentialism_school, beneficiary,
    powerful, generational, constrained, global).

% Enforce clinical protocols grounded in immutable biological sex: chromosomal testing, reproductive anatomy assessment, medical record sex designation tied to birth assignment. They benefit from the clarity (biological sex is objective and measurable) and from the legal standing of medical authority to certify sex category. Pressure to recognize gender identity as primary category would require clinical protocol reform and loss of medical gatekeeping authority over category determination.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, medical_gatekeepers_sex_essentialism_alignment, agenda_setter,
    institutional, generational, constrained, national).

% Codify category membership through law: birth certificate sex designation, legal sex category as immutable except through extraordinary procedure (or not mutable at all). They administer the boundary through legal document systems, sex-segregated legal regimes (prisons, sports eligibility, immigration processing), and court recognition of biological sex as the canonical category marker. Shift to identity-based determination would require statutory revision and loss of the clarity and enforceability of biological criteria.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, legal_systems_biological_sex_doctrine, agenda_setter,
    institutional, generational, analytical, national).

% Argue from outside the constraint that category membership should track self-identification and lived gender, not biological markers at birth. They dispute the biological-sex reading's framing of immutability and its exclusionary consequences. They conduct research, file legal challenges, and produce competing discourse that unsettles the biological-sex reading's authority.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, identity_cohesion_advocates, observer,
    organized, biographical, analytical, global).

% Have immutable biological markers that do not fit the binary sex category (chromosomal, gonadal, or phenotypic variation). They are structurally excluded from the conversation between cis women and trans women because the biological-sex reading leaves no coherent place for non-binary sex: it forces assignment to one binary category (usually via medical intervention at birth) despite biological ambiguity. They would advocate for recognition that biological sex is not a clean binary, which would undermine the categorical boundary the biological-sex reading defends.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, intersex_and_ambiguous_sex_individuals, excluded,
    powerless, biographical, trapped, global).

% Hold a reading of 'woman' grounded in gender identity and social recognition, not immutable biological sex. They argue that excluding trans women harms feminist coalition and that gender identity is a legitimate basis for category membership. They form a competing organized voice within feminist and women's spaces, contesting the biological-sex boundary from within.
narrative_ontology:constraint_stakeholder(gendered_category_membership__biological_sex_reading, cis_women_feminist_identity_reading_alignment, observer,
    organized, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__biological_sex_reading, cis_women_as_category_boundary_defenders).
narrative_ontology:fixing_cost_class(gendered_category_membership__biological_sex_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Organizes the institutional architecture of sex-segregated space and feminist knowledge production by establishing an objective, immutable criterion (biological sex at birth) for category membership. This avoids the problem of fraudulent entry (people claiming 'woman' status for resource capture or predatory purposes) and grounds feminist analysis in a shared material fact. The coordination problem solved is: 'How can we maintain the coherence and integrity of spaces and knowledge frameworks built around sex-based commonality without allowing category boundary dissolution?'
% TRANSFER_FUNCTION: Moves the social authority to define 'woman' from individuals' self-understanding and social recognition to biological markers and institutional gatekeepers (legal systems, medical authorities, category boundary defenders). Transfer flows from trans women and ambiguous-sex individuals (who lose category recognition) to cis women and institutional administrators (who gain control over category certification and sex-segregated space regulation).
% ABSENT_VOICES: Intersex and ambiguous-sex individuals have no seat at the table: the binary sex criterion leaves no category place for them except forced assignment. Trans men and non-binary individuals are also structurally absent, though their absence is less visible (trans men are often treated as women-who-left, and non-binary identities are treated as incoherent rather than excluded). Gender-identity advocates do appear in public discourse but lack institutional gatekeeping power and are often positioned as external critics rather than participants in the space design.
% DISAPPEARANCE_RATIONALE: The biological-sex reading claims the constraint would vanish overnight: without immutable biological sex as criterion, sex-segregated spaces would lose coherence, feminist knowledge claims about sex-based oppression would lose grounding, and predatory entry into 'woman' spaces would become trivial. Critics contest this: they argue sex-segregated spaces could cohere around gender identity instead, feminist analysis could extend to gender-based oppression as well as sex-based, and institutional safeguards (background checks, community accountability) could manage entry without biological gatekeeping. The divergence is not empirical but structural: it depends on whether the constraint is foundational (without it the whole architecture collapses) or ornamental (maintaining it against alternatives requires constant enforcement).
% FOUNDING_PROBLEM: Early feminist movement needed to define 'woman' in objective, un-deniable terms to ground claims about women's shared material oppression (reproductive labor, sexual violence, economic discrimination) and to justify exclusion of men from women's spaces and knowledge production. Biological sex (chromosomes, reproductive anatomy, reproductive capacity) offered an apparently objective criterion immune to individual dispute. The constraint was built to solve: 'How can we make the category 'woman' credible and un-contestable as a basis for political and intellectual community?'
% FOUNDING_PROBLEM_CORROBORATION: Feminist theorists and category boundary defenders attest the founding problem is still live: sex-based oppression persists and requires coherent category grounding. Critics (trans advocates, intersex advocates, identity-reading feminists) attest the problem is substantially solved and the constraint now persists for gatekeeping and symbolic control: identity-based recognition and institutional safeguards accomplish the space-integrity and knowledge-coherence goals without exclusion. Legal scholar Jennifer Tuohey and medical anthropologist Gabriel Dorland, writing from outside the beneficiary set, document the shift: early constraints were about inclusion verification; contemporary enforcement is increasingly about exclusion boundary maintenance. Bioethicists outside feminist gatekeeping circles attest that immutable-biological-sex criterion does not track observable biology cleanly (chromosomal, gonadal, phenotypic variation is continuous and ambiguous), suggesting the criterion solves institutional closure rather than correspondence to fact.
narrative_ontology:disappearance_verdict(gendered_category_membership__biological_sex_reading, contested).
narrative_ontology:founding_problem_status(gendered_category_membership__biological_sex_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__biological_sex_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(gendered_category_membership__biological_sex_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__biological_sex_reading, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__biological_sex_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__biological_sex_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__biological_sex_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.78) because the constraint transfers category authority from trans women and ambiguous-sex individuals to institutional gatekeepers and cis women boundary defenders, who gain control over who is admitted to 'woman' category and spaces. This transfer is sustained by suppression (0.71): medical and legal systems enforce category compliance; social ostracism and institutional barriers suppress exit and alternative self-identification; identity-locked psychology makes exit costly (identity fusion). Theater ratio is moderate (0.42): some constraint activity is genuine space-design and knowledge-grounding (coordination function), but an increasing share is boundary patrol and exclusionary enforcement against trans and non-binary individuals. Resistance is high (0.73) because identity advocates and trans women actively contest the boundary through legal challenges, institutional pressure, and competing discourse. The measurement series show steady extraction intensification over 40 years: as identity-reading challenges mounted, enforcement machinery hardened (suppression requirement rose), and more of the constraint's activity became overtly boundary-patrol (theater ratio rose, then stabilized). At t=0 (founding era), the constraint carried more genuine coordination function; at t=40 (contemporary), it carries more gatekeeping and exclusion.
 *
 * PERSPECTIVAL GAP:
 *   The boundary-defender seat (cis women, feminist theorists, institutional gatekeepers) computes this as Rope or Scaffold: they see genuine coordination (sex-segregated space, coherent feminist knowledge) with temporary enforcement overhead. The payer seat (trans women) computes this as Snare: the coordination rationale is cover for identity suppression and institutional exclusion. The space-designer seat sits between: they see genuine coordination need but increasing cost and legal liability from enforcement. The engine's per-seat classification will diverge because the structural data (beneficiary/victim, power, exit_options, spatial_scope) encode these different positions. The claim is Tangled Rope because the constraint simultaneously coordinates (provides sex-segregated space and knowledge coherence) and extracts (excludes, suppresses alternative identity, concentrates categorical authority). Divergence from the claim would signal that one function dominates — if the computed type approaches Snare, extraction dominates; if it approaches Rope, coordination dominates.
 *
 * DIRECTIONALITY LOGIC:
 *   Trans women have directionality near 1.0 (full target): they are the named victims, have powerless institutional position, identity-locked exit (cannot exit by changing self-identification without the boundary defenders' recognition), and face global-scope enforcement. Cis women boundary defenders have directionality near 0.0 (full beneficiary): they collect category authority, have organized power, constrained but not trapped exit (they could theoretically accept identity-based inclusion without losing cis identity, though they resist), and are seated in global-scope institutional structures. Feminist theorists and medical gatekeepers: moderate-to-low directionality (beneficiaries from the constraint's persistence, powerful institutional position, analytical exit — they could theoretically pivot to identity-based frameworks without loss). Space designers: near-symmetric (0.5): they gain institutional clarity but pay operational costs; constrained exit; national scope. Intersex individuals: high directionality approaching trans women (excluded, powerless, trapped, but less visible).
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint exhibits mandatrophy tension: its founding problem was 'how can we ground feminist analysis in objective material fact (sex)?' and its founding solution was 'immutable biological sex as category membership.' The founding problem's status is contested because identity-reading alternatives now exist and carry evidential support (gender identity as psychologically constitutive, sex markers as continuous rather than binary, feminist analysis as compatible with gender-based oppression claims). Contemporary enforcement increasingly looks like boundary maintenance against alternative readings (trans women, intersex individuals, identity advocates) rather than original-problem solving. The constraint persists not because the original problem demands it, but because beneficiary seats (institutional gatekeepers, boundary defenders) have invested authority and resources in maintaining the boundary. A Mandatrophy Resolved verdict would require either: (1) the founding problem to be demonstrated dead (identity-based recognition solves space integrity and knowledge coherence without exclusion), OR (2) the constraint to be redefined as Snare (pure extraction with a coordination cover story). Neither has occurred; the constraint remains in Mandatrophy Unresolved state — the founding coordination function is contested, the exclusion is substantive, and enforcement is intensifying rather than diminishing.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_sex_continuity_and_measurement,
    'Is biological sex a binary, stable category measurable at birth and unchanging across an individual''s lifetime? Or is sex a continuous multidimensional variable (chromosomal, gonadal, phenotypic, hormonal) that is ambiguous, variable, and subject to change?',
    'Systematic medical and biological survey: chromosomal screening of birth cohorts, comparison with phenotypic and hormonal markers, tracking of inter-individual variation and within-individual change over time. Comparative anatomy and embryology studies on sex determination and differentiation pathways.',
    'If sex is binary and stable, the biological-sex reading''s criterion is defensible as objective and immutable. If sex is continuous and ambiguous, the binary criterion requires medical force (intersex assignment) or institutional closure (rounding ambiguous cases into binary categories), which would reclassify the constraint from Rope/Tangled Rope (genuine coordination problem + some asymmetry) toward Snare (gatekeeping and exclusion are primary functions, not side effects).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_sex_continuity_and_measurement, empirical, 'Whether biological sex is a clean binary or a continuous, ambiguous variable.').

omega_variable(
    sex_segregation_necessity_and_substitutability,
    'Is sex-segregated institutional design necessary to protect the integrity and function of spaces like bathrooms, shelters, prisons, sports, intimate care? Or can gender-identity or other criteria accomplish the same protective and functional goals?',
    'Comparative institutional analysis: jurisdictions that have implemented identity-based or hybrid-criterion access policies (some US states, Canada, Spain, Argentina); outcome measurement on space safety, accessibility, institutional function; staff and occupant interview data on perceived changes in dynamics and feasibility.',
    'If sex segregation is necessary, the biological-sex reading solves a genuine institutional coordination problem and the constraint is legitimately Tangled Rope (with asymmetric enforcement costs). If gender-identity or other criteria accomplish the goals equally well, the sex-segregation framework becomes a choice rather than a necessity, and the persistence of biological-sex criteria appears more extractive (driven by gatekeeping preference rather than functional requirement). This would shift classification toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sex_segregation_necessity_and_substitutability, empirical, 'Whether sex-segregation is institutionally necessary or one option among functional alternatives.').

omega_variable(
    feminist_knowledge_grounding_sex_dependency,
    'Does feminist analysis of oppression depend logically on sex as the primary category? Or can feminist analysis extend to gender-based oppression (which would not require sex-category coherence)?',
    'Meta-analysis of feminist theory: document which feminist claims require sex-category specificity and which extend to gender-based formulations. Study empirical feminist research on sex-based and gender-based oppression to determine if both are coherent and supported by evidence.',
    'If feminist analysis is sex-dependent, the constraint''s coordination function (knowledge grounding) is real. If feminist analysis can extend to gender-based oppression without sex-category bifurcation, the knowledge-grounding rationale is a cover for categorical gatekeeping, and the constraint appears more extractive. Classification would shift from Tangled Rope toward Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feminist_knowledge_grounding_sex_dependency, conceptual, 'Whether feminist knowledge production requires sex-category coherence or can extend to gender-based formulations.').

omega_variable(
    identity_internalization_vs_structural_suppression,
    'For trans women and ambiguous-sex individuals excluded under the biological-sex reading, is the suppression of category recognition primarily structural (external barriers: medical, legal, institutional, social) or internalized (psychological acceptance of the exclusionary category assignment)?',
    'Longitudinal study of individuals who have transitioned between regimes (regions that enforce biological-sex criteria to regions that recognize identity-based category membership): measure identity confidence, self-category identification, and psychological distress pre- and post-transition. Disentangle external barrier removal from identity persistence.',
    'If suppression is primarily structural, removal of external barriers would restore identity recognition and reduce extractiveness; the constraint would be reclassifiable if enforcement changed. If suppression is partly internalized, the constraint''s effective extraction persists even after external enforcement ends — the target carries the suppression internalized, making the constraint''s total extraction higher than structural measures suggest and more persistent than mere institutional change would remedy.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_internalization_vs_structural_suppression, empirical, 'Whether category suppression is structural or internalized.').

omega_variable(
    kernel_foreclosure_between_readings,
    'Do the biological-sex reading and the gender-identity reading logically foreclose each other, or do they coexist as live options across different institutional sectors and parties?',
    'Institutional analysis: document sectors where both readings are simultaneously operative (e.g., some hospitals use identity-based recognition, others use biological-sex criteria; some legal jurisdictions recognize identity, others enforce biological sex; some feminist organizations accept trans women, others exclude them). If both readings persist without one logically compelling the other, coexistence is the answer; if institutional dynamics show one reading actively displacing the other, foreclosure may be occurring.',
    'This omega locates the committer disagreement. If readings coexist across sectors, the constraint should be decomposed into multiple sector-specific stories (one per jurisdiction or institutional context), each measuring extraction differently. If one reading forecloses the other within a framework, the single-reading assumption breaks down and the constraint requires cross-reading analysis. For this story (biological-sex reading alone), the omega signals that institutional divergence makes the global claim partially incoherent — the constraint''s operation depends on which institutional sector is examined.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_foreclosure_between_readings, conceptual, 'Whether biological-sex and gender-identity readings logically foreclose each other or coexist across sectors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__biological_sex_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__biological_sex_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement(gend_tr_t5, gendered_category_membership__biological_sex_reading, theater_ratio, 5, 0.28).
narrative_ontology:measurement(gend_tr_t10, gendered_category_membership__biological_sex_reading, theater_ratio, 10, 0.32).
narrative_ontology:measurement(gend_tr_t15, gendered_category_membership__biological_sex_reading, theater_ratio, 15, 0.36).
narrative_ontology:measurement(gend_tr_t20, gendered_category_membership__biological_sex_reading, theater_ratio, 20, 0.39).
narrative_ontology:measurement(gend_tr_t25, gendered_category_membership__biological_sex_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__biological_sex_reading, theater_ratio, 30, 0.42).
narrative_ontology:measurement(gend_tr_t40, gendered_category_membership__biological_sex_reading, theater_ratio, 40, 0.42).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__biological_sex_reading, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gend_be_t5, gendered_category_membership__biological_sex_reading, base_extractiveness, 5, 0.58).
narrative_ontology:measurement(gend_be_t10, gendered_category_membership__biological_sex_reading, base_extractiveness, 10, 0.64).
narrative_ontology:measurement(gend_be_t15, gendered_category_membership__biological_sex_reading, base_extractiveness, 15, 0.69).
narrative_ontology:measurement(gend_be_t20, gendered_category_membership__biological_sex_reading, base_extractiveness, 20, 0.73).
narrative_ontology:measurement(gend_be_t25, gendered_category_membership__biological_sex_reading, base_extractiveness, 25, 0.76).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__biological_sex_reading, base_extractiveness, 30, 0.77).
narrative_ontology:measurement(gend_be_t40, gendered_category_membership__biological_sex_reading, base_extractiveness, 40, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__biological_sex_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement(gend_su_t5, gendered_category_membership__biological_sex_reading, suppression_requirement, 5, 0.54).
narrative_ontology:measurement(gend_su_t10, gendered_category_membership__biological_sex_reading, suppression_requirement, 10, 0.6).
narrative_ontology:measurement(gend_su_t15, gendered_category_membership__biological_sex_reading, suppression_requirement, 15, 0.65).
narrative_ontology:measurement(gend_su_t20, gendered_category_membership__biological_sex_reading, suppression_requirement, 20, 0.68).
narrative_ontology:measurement(gend_su_t25, gendered_category_membership__biological_sex_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__biological_sex_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement(gend_su_t40, gendered_category_membership__biological_sex_reading, suppression_requirement, 40, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__biological_sex_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(gendered_category_membership__biological_sex_reading, 0.12).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__gender_identity_reading).
narrative_ontology:affects_constraint(gendered_category_membership__biological_sex_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% The gendered_category_membership kernel decomposes into three constraint stories, each instantiating a different reading. The biological-sex reading (this story) grounds category membership in immutable biological markers; the gender-identity reading grounds it in self-identification; the social-role reading grounds it in performance and recognition. Each reading has distinct ε, beneficiary/victim structures, and extracted outcomes. The readings coexist across different institutional sectors and are held by different organized parties simultaneously — they do not logically foreclose each other globally, only within particular frameworks (e.g., within a single jurisdiction's legal system, one reading typically dominates). All three stories should be generated and linked via network.affects_constraints to enable cross-reading analysis of institutional divergence and kernel contest dynamics.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gendered_category_membership__biological_sex_reading, organized, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
