% ============================================================================
% CONSTRAINT STORY: woman_female_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__sex_biology_reading, []).

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
 *   constraint_id: woman_female_category__sex_biology_reading
 *   human_readable: Female Category Definition via Sex Biology
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested 'woman/female'
 *   category kernel: the sex_biology_reading, which determines membership by
 *   chromosomal sex (XX/XY), reproductive anatomy, and developmental biology.
 *   Under this reading, category membership is objectively verifiable (from
 *   birth certificates, medical records, or chromosomal analysis) and not
 *   subject to self-identification override. This reading benefits natal
 *   females seeking sex-based legal protections (anti-discrimination law,
 *   reproductive autonomy, domestic violence refuges); it extracts from trans
 *   women and sex-nonconforming individuals by excluding them from
 *   sex-segregated spaces and from the legal recognition that category
 *   membership confers. The constraint is claimed as tangled_rope: it
 *   coordinates sex-segregated-space access and grounds sex-based rights
 *   (genuine coordination function) while asymmetrically extracting from
 *   trans people (active enforcement to exclude). The authoring claim and
 *   metrics are independent: this constraint is CLAIMED as tangled_rope AND
 *   the metrics describe substantially extractive, actively enforced
 *   operation. The measurement series show extractiveness rising over the
 *   interval (from 0.45 to 0.68) as the category boundary is litigated and
 *   institutions invest enforcement machinery (case law, institutional
 *   policies, verification procedures); extraction plateaus once institutions
 *   standardize. Theater_ratio rises initially (new policies require
 *   justification and debate) then plateaus as enforcement normalizes (the
 *   theater becomes routine).
 *
 * KEY AGENTS:
 *   - natal_females_seeking_sex_based_protections: benefit from sex-biological category definition; organized, constrained exit, national scope
 *   - trans_women: excluded from female-only spaces; powerless, identity-locked exit, national scope
 *   - sex_nonconforming_individuals: caught between biological-sex enforcement and identity-recognition denial; powerless, identity-locked exit
 *   - prison_and_shelter_administrators: enforce the biological-sex placement rule; institutional, constrained by conflicting legal directives
 *   - feminist_advocacy_organizations: mobilize around sex-based rights; organized, benefit from institutional codification of biological framing
 *   - trans_rights_advocacy_organizations: excluded from agenda-setting; organized, would reshape constraint if given standing
 *   - medical_professionals_and_researchers: provide expertise on sex-biology-identity relationships; analytical, institutional
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, 0.68).
domain_priors:suppression_score(woman_female_category__sex_biology_reading, 0.71).
domain_priors:theater_ratio(woman_female_category__sex_biology_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(woman_female_category__sex_biology_reading, resistance, 0.79).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__sex_biology_reading, "Female Category Definition via Sex Biology").
narrative_ontology:topic_domain(woman_female_category__sex_biology_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__sex_biology_reading, 'b9a3a318-3873-48c2-88ad-25571b53f5dd').
narrative_ontology:cs_kernel_codification('b9a3a318-3873-48c2-88ad-25571b53f5dd', fixed_text).
narrative_ontology:cs_authority_grounding('b9a3a318-3873-48c2-88ad-25571b53f5dd', lineage).
narrative_ontology:cs_interpretation_layer_present('b9a3a318-3873-48c2-88ad-25571b53f5dd').
narrative_ontology:cs_reading_relation('b9a3a318-3873-48c2-88ad-25571b53f5dd', woman_female_category__gender_identity_reading, coexists_with).
narrative_ontology:cs_reading_relation('b9a3a318-3873-48c2-88ad-25571b53f5dd', woman_female_category__hybrid_contextual_reading, influences).
narrative_ontology:cs_axiom('b9a3a318-3873-48c2-88ad-25571b53f5dd', foundational, biological_sex_determines_category_membership).
narrative_ontology:cs_axiom_status(biological_sex_determines_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('b9a3a318-3873-48c2-88ad-25571b53f5dd', biological_sex_determines_category_membership, empirically_contingent).
narrative_ontology:cs_axiom('b9a3a318-3873-48c2-88ad-25571b53f5dd', foundational, sex_segregation_necessary_for_female_safety).
narrative_ontology:cs_axiom_status(sex_segregation_necessary_for_female_safety, holdable).
narrative_ontology:cs_axiom_grounding('b9a3a318-3873-48c2-88ad-25571b53f5dd', sex_segregation_necessary_for_female_safety, instrumental).
narrative_ontology:cs_reference_frame('b9a3a318-3873-48c2-88ad-25571b53f5dd', feminist_rights_framework_sex_based).
narrative_ontology:cs_drift_state('b9a3a318-3873-48c2-88ad-25571b53f5dd', contemporary_trans_recognition_era, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('b9a3a318-3873-48c2-88ad-25571b53f5dd', '').
narrative_ontology:cs_kernel_id(woman_female_category__sex_biology_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, trans_women).
narrative_ontology:constraint_victim(woman_female_category__sex_biology_reading, sex_nonconforming_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__sex_biology_reading, feminist_advocacy_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Access sex-segregated spaces (prisons, shelters, locker rooms, domestic violence refuges) based on female biological sex, justified by physical safety, medical privacy, and protection from predation. Argue that reproductive anatomy and developmental history create shared vulnerabilities requiring spaces where no person with male anatomy or testosterone history is present. Benefits from exclusionary policies that treat sex-based categories as legally stable and enforceable.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, natal_females_seeking_sex_based_protections, beneficiary,
    organized, generational, constrained, national).

% Excluded from female-only spaces despite female gender identity and in many cases hormone therapy or surgical transition. Face barriers to incarceration in female prisons (risking male-facility violence), denial of access to women's shelters during homelessness, exclusion from women's locker rooms and bathrooms. Identity-locked because exit would require abandoning their core self-understanding; constrained by institutional policy and legal precedent that treats sex category as chromosomal/anatomical.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_women, payer,
    powerless, biographical, identity_locked, national).

% Face enforcement pressure to conform bodies and behavior to sex-category expectations; excluded from spaces matching their gender identity when sex biology does not align. Caught between biological-sex enforcement and identity-recognition denial. Identity-locked by enforcement of chromosomal/anatomical categories against their expressed identity.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, sex_nonconforming_individuals, payer,
    powerless, biographical, identity_locked, national).

% Manage sex-segregated facilities under legal frameworks that treat sex category as determined by biology (birth certificate, chromosomal records, or in some jurisdictions medical records). Enforce placement rules to satisfy statutory sex-segregation mandates. Operate under litigation risk: pressure from natal-female advocates for strict biological enforcement, and counter-pressure from trans-rights advocates and civil liberties organizations for identity recognition. Constrained by conflicting legal directives and funding oversight.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, prison_and_shelter_administrators, agenda_setter,
    institutional, biographical, constrained, national).

% Mobilize around sex-based rights, arguing that sex categories rooted in reproductive anatomy and developmental biology are the material basis for women's oppression and therefore the proper legal anchor for women's protections. Frame the constraint as grounding hard-won sex-based legal rights in biology rather than performance. Lobby legislatures and administrative agencies to codify sex-biological definitions and resist identity-based category redefinition. Benefit from legal and social validation of their framing.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, feminist_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(woman_female_category__sex_biology_reading, feminist_advocacy_organizations, agenda_setter).

% Argue that self-identification is the legitimate basis for category membership; view biological determinism as denying trans people's core identity and as instrumentalizing biology to exclude trans people from rights and services. Excluded from agenda-setting in institutions and legal frameworks that treat sex biology as the category anchor. Would reshape the constraint entirely if given institutional standing.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, trans_rights_advocacy_organizations, excluded,
    organized, generational, mobile, national).

% Provide expertise on the relationship between chromosomal sex, hormone profiles, reproductive anatomy, developmental history, and health outcomes. Their research and testimony are cited by both sides—some studies find persistent health differences between natal sexes even post-transition; others emphasize the plasticity of sex-related traits and the medical risk of exclusion from appropriate care. Analytical seat; expertise is contested and deployed by multiple parties.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, medical_professionals_and_researchers, observer,
    institutional, generational, analytical, global).

% Monitor both sex-based protections for natal females and equal treatment/non-discrimination rights for trans people and sex-nonconforming individuals. Face structural tension: recognizing sex-based categories risks discrimination claims from trans people; recognizing only identity-based categories risks denying sex-based harms to natal females. Analytical seats that eventually feed into legal and policy decisions.
narrative_ontology:constraint_stakeholder(woman_female_category__sex_biology_reading, civil_liberties_and_human_rights_bodies, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__sex_biology_reading, feminist_advocacy_organizations).
narrative_ontology:fixing_cost_class(woman_female_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a stable, legally recognizable boundary for sex-segregated spaces (prisons, shelters, locker rooms, domestic violence refuges) where people with female reproductive anatomy and developmental history can access protections and services designed around their specific vulnerabilities and medical needs. Anchors sex-based legal rights (reproductive autonomy, anti-discrimination law, domestic violence law) in a biological referent claimed to be objective and difficult to fake or game.
% TRANSFER_FUNCTION: Moves access to sex-segregated spaces and sex-based legal protections from trans women and sex-nonconforming individuals to natal females. Transfers the burden of proof-of-identity (and risk of exclusion) from institutions onto those seeking access, who must disclose or prove chromosomal/anatomical status. Transfers the legitimacy of sex-based category maintenance from individual identity to institutional verification of biology.
% ABSENT_VOICES: Trans people and non-binary individuals are structurally excluded from agenda-setting even though they bear the direct costs of enforcement. Their voices appear only as the object of policy and litigation, not as co-authors of the category framework. Sex-segregated-space users who do not conform neatly to the biological definition (intersex individuals, trans men, non-binary people assigned female at birth) have marginal standing.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared—if sex-category membership were determined solely by self-identification or by context-dependent rules rather than chromosomal/anatomical biology—sex-segregated spaces would reorganize: some would close, others would shift to context-specific rules or open access by identity. Legal frameworks anchoring sex-based rights would require rebuilding. The distribution of access to protection and privacy would shift substantially, as would the institutional enforcement machinery that verifies identity.
% FOUNDING_PROBLEM: Women face sex-specific violence, privacy needs, and medical vulnerabilities (menstruation, pregnancy, sexual assault). Early feminist legal strategy anchored women's rights (bodily autonomy, rape law, pregnancy protections, domestic violence shelter access) in sex as a biological category rooted in reproductive anatomy and developmental history, to make the category difficult to erase and to ground rights in material differences rather than subjective identity.
% FOUNDING_PROBLEM_CORROBORATION: Feminist advocates and natal-female survivors of violence attest the founding problem remains urgent: sex-specific vulnerabilities persist; they argue biological grounding is essential to preventing the category from dissolving and rights from erosion. Trans rights advocates and medical researchers attest the problem is partially reframed: sex-specific vulnerabilities exist but do not require exclusion of all trans women; they cite research showing trans women's health and safety needs are different from those of trans men and non-binary people, and that inclusive policies can coexist with sex-specific protections. Legal scholars from outside the benefiting parties (civil liberties organizations, international human rights bodies) attest both claims have merit and the founding problem has mutated: it is now a problem of managing multiple overlapping vulnerabilities and rights claims simultaneously, not a stable problem with one solution.
narrative_ontology:disappearance_verdict(woman_female_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(woman_female_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__sex_biology_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68 at end) is high because the constraint's enforcement explicitly denies trans women and sex-nonconforming individuals access to spaces and legal categories they seek, and this denial is grounded in a boundary they cannot cross (chromosomal sex is not changeable). The constraint is actively enforced—administrators verify sex status, deny access, sometimes use physical examination or legal force. Suppression is high (0.71) because the constraint requires trans people to either accept exclusion or engage in costly, risky defiance (legal action, facility rule violation, identity concealment). Theater_ratio is moderate (0.28): the security and privacy justifications for sex segregation are partially real (sexual assault in prisons is documented; sex-specific medical needs exist), but institutional behavior shows increasing theater—policies become more elaborate and rhetorically defended as media and litigation pressure rises; enforcement becomes less about genuine coordination and more about boundary defense. Accessibility_collapse is moderate (0.62): for trans women, the alternative to acceptance is costly litigation or exit to other jurisdictions; for natal females, the alternative is abandonment of sex-segregated spaces. Resistance is high (0.79): trans rights advocates, civil liberties organizations, and medical professionals mount consistent resistance to biological-determinism framing; litigation is chronic; legislative efforts to codify biological definition are met with legislative counteroffort for identity-recognition bills. The measurement trajectory shows a rapid buildup of extractiveness and suppression (t0–t20, litigation period) followed by plateau (t20–t35, institutions standardize policy). This is characteristic of institutional crystallization: initial contestation gives way to administrative routinization.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats (trans_women, sex_nonconforming_individuals) and the beneficiary/agenda_setter seats (natal_females, administrators, feminist organizations) compute different types from the same structural data. From the beneficiary seat, the constraint is rope: genuine coordination (sex-segregated spaces solve real coordination problems around safety and privacy) plus necessary enforcement (boundaries must be verified). From the target seats, the constraint computes as snare: the stated coordination function (preventing predatory male presence in women's spaces) could be achieved via behavioral policies and risk assessment rather than categorical exclusion of all trans women; the enforcement persists because it serves rent-collection (institutional simplicity, legal safeguards for natal-female interests) rather than safety. The engine computes per-seat types from the structural power/exit/beneficiary/victim data; this reading declares the structural asymmetry (payers have identity-locked exit, beneficiaries have arbitrage/mobile exit) and allows the engine to detect the seat divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Natal_females are direct beneficiaries (d near 0.0, negative χ: the constraint subsidizes their access to protections). Trans_women are direct targets (d near 1.0, positive χ: the constraint extracts from them by denying access they seek and enforcing identity verification at risk of physical or legal harm). Sex-nonconforming_individuals are also targets (d near 1.0) because enforcement of the biological boundary applies to them. Prison/shelter administrators are agenda_setters constrained by legal mandates (d somewhere between 0.4–0.6: they implement the boundary but are not primary beneficiaries; they would prefer less enforcement cost). Feminist organizations are secondary beneficiaries (d near 0.1–0.2: they benefit from institutional recognition of their framing but do not directly collect from the constraint). Trans rights organizations are excluded (d undefined by derivation; they would be targets if inside the system). The directionality is stable across the measurement interval because the structural relationships (who benefits, who bears costs, who has exit) do not change—extractiveness rises because enforcement infrastructure matures, not because directionality shifts.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (sex-specific vulnerabilities requiring spaces where predatory males are absent) is CONTESTED: natal-female advocates attest it remains live and urgent; trans rights advocates attest it is partially solved by non-categorical approaches (behavioral screening, risk assessment, trans-woman-inclusive policies with safety controls) and that the categorical solution causes unacceptable harms. The disappearance_verdict (world_rearranges) pairs with status=contested to signal a zombie/mandatrophy candidate: if the constraint disappeared, sex-segregated facilities would reorganize—some would close, others would adopt context-or-identity-dependent rules. The finding that extractiveness has plateaued over t20–t35 (stabilizing at 0.68) while theater_ratio and suppression_requirement stabilized earlier (t25 onward) suggests institutions have settled on a stable enforcement equilibrium rather than moving toward either resolution (abolition or deeper integration). This is piton territory—the constraint persists because the cost of change (institutional reconfiguration, legal conflict, abandonment of the natal-female-protection framing) exceeds the concentrated benefit to any one party of fixing it, even though the constraint's foundational mandate is contested. However, the constraint is not yet fully piton because organized beneficiary advocates (feminist organizations) maintain institutional and legislative investment in preserving the sex-biological boundary; if that investment eroded, theater_ratio would rise sharply as enforcement became purely performative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the category ''woman'' or ''female'' an objective biological kind (chromosomal sex, reproductive anatomy, developmental history) or a social/legal category that borrows the language of biology without being fully determined by it?',
    'This is a committer/reading choice, not an empirical question. This constraint instantiates the sex_biology_reading; the gender_identity_reading and hybrid_contextual_reading are sibling constraints (other files) representing competing framings of the same kernel (the category boundary). No single empirical fact resolves which framing is correct—the framing is chosen by the kernel-reading, and different readings author different ε values for the standing arrangement under contest.',
    'This reading treats category membership as biologically determined and enforces that boundary; a sibling reading would treat membership as identity-determined and would author a different ε for the standing arrangement (higher extraction on trans people under this reading, lower extraction under the identity-reading). The three constraint files together model the irreducible contest.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'This constraint is ONE READING of a contested kernel; the reading/framing choice is author-declared, not engine-computed.').

omega_variable(
    suppression_internalization,
    'To what degree is the suppression of trans women and sex-nonconforming individuals measured in this constraint structural (institutional barriers, legal enforcement, facility design) versus internalized (acceptance of the framework that biology determines category membership)?',
    'Post-enforcement trajectory: if suppression persists when institutional enforcement is removed, reclassify as partially internalized. Measure willingness to challenge the category framework independent of enforcement pressure.',
    'If suppression is highly internalized, the constraint''s effective suppression is higher than the structural measure suggests—trans people carry the constraint with them after exit (e.g., internalized shame, acceptance of exclusion as legitimate). If suppression is mainly structural, opening institutional enforcement would reduce effective suppression more readily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization, empirical, 'Structural vs. internalized suppression mechanism in category enforcement.').

omega_variable(
    biological_sex_determinacy,
    'Are chromosomal sex (XX/XY), reproductive anatomy, and developmental hormone history discrete, non-overlapping categories, or do they vary independently such that some people fall into mixed or ambiguous categories?',
    'Epidemiological study of sex-variance: intersex conditions, androgen insensitivity, sex chromosome variations (XXY, XYY, X0). If these conditions are rare but non-negligible, the biological boundary is fuzzier than the constraint assumes.',
    'If biological sex is cleanly binary, enforcement is straightforward and the constraint''s extractiveness is stable. If biological sex is a continuum or has common mixed-category cases (intersex people), enforcement requires arbitration (which anatomy counts? which hormone levels? which documents?), raising theater_ratio and effective suppression—the boundary becomes contestable even within biological framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(biological_sex_determinacy, empirical, 'Whether chromosomal sex, reproductive anatomy, and hormone profile form a stable, discrete category or vary independently.').

omega_variable(
    alternative_reading_coexistence,
    'Can the gender_identity_reading and hybrid_contextual_reading coexist with this sex_biology_reading within a single institutional framework, or does adopting one foreclose the others?',
    'Jurisdictional survey: identify jurisdictions that hold each reading and examine whether they can coexist peacefully or whether one inevitably displaces the other over time. Examine constitutional and statutory law to see if courts treat readings as mutually exclusive or as context-dependent tradeoffs.',
    'If readings foreclose each other, the constraint system is a zero-sum winner-take-all competition, and institutional stability requires consolidation to one reading. If readings coexist (different contexts, different policies for different populations), institutional strain is chronic but workable. This affects the durability and theater_ratio of the constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_reading_coexistence, conceptual, 'Whether competing readings of the kernel can coexist institutionally or necessarily displace each other.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__sex_biology_reading, 0, 35).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__sex_biology_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__sex_biology_reading, theater_ratio, 5, 0.16).
narrative_ontology:measurement_basis(woma_tr_t5, observed).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__sex_biology_reading, theater_ratio, 10, 0.19).
narrative_ontology:measurement_basis(woma_tr_t10, observed).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__sex_biology_reading, theater_ratio, 15, 0.23).
narrative_ontology:measurement_basis(woma_tr_t15, observed).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__sex_biology_reading, theater_ratio, 20, 0.26).
narrative_ontology:measurement_basis(woma_tr_t20, observed).
narrative_ontology:measurement(woma_tr_t25, woman_female_category__sex_biology_reading, theater_ratio, 25, 0.27).
narrative_ontology:measurement_basis(woma_tr_t25, observed).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__sex_biology_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(woma_tr_t30, observed).
narrative_ontology:measurement(woma_tr_t35, woman_female_category__sex_biology_reading, theater_ratio, 35, 0.28).
narrative_ontology:measurement_basis(woma_tr_t35, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__sex_biology_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t5, woman_female_category__sex_biology_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement_basis(woma_be_t5, observed).
narrative_ontology:measurement(woma_be_t10, woman_female_category__sex_biology_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(woma_be_t10, observed).
narrative_ontology:measurement(woma_be_t15, woman_female_category__sex_biology_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(woma_be_t15, observed).
narrative_ontology:measurement(woma_be_t20, woman_female_category__sex_biology_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(woma_be_t20, observed).
narrative_ontology:measurement(woma_be_t25, woman_female_category__sex_biology_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(woma_be_t25, observed).
narrative_ontology:measurement(woma_be_t30, woman_female_category__sex_biology_reading, base_extractiveness, 30, 0.68).
narrative_ontology:measurement_basis(woma_be_t30, observed).
narrative_ontology:measurement(woma_be_t35, woman_female_category__sex_biology_reading, base_extractiveness, 35, 0.68).
narrative_ontology:measurement_basis(woma_be_t35, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__sex_biology_reading, suppression_requirement, 0, 0.54).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t5, woman_female_category__sex_biology_reading, suppression_requirement, 5, 0.59).
narrative_ontology:measurement_basis(woma_su_t5, observed).
narrative_ontology:measurement(woma_su_t10, woman_female_category__sex_biology_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(woma_su_t10, observed).
narrative_ontology:measurement(woma_su_t15, woman_female_category__sex_biology_reading, suppression_requirement, 15, 0.68).
narrative_ontology:measurement_basis(woma_su_t15, observed).
narrative_ontology:measurement(woma_su_t20, woman_female_category__sex_biology_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement_basis(woma_su_t20, observed).
narrative_ontology:measurement(woma_su_t25, woman_female_category__sex_biology_reading, suppression_requirement, 25, 0.71).
narrative_ontology:measurement_basis(woma_su_t25, observed).
narrative_ontology:measurement(woma_su_t30, woman_female_category__sex_biology_reading, suppression_requirement, 30, 0.71).
narrative_ontology:measurement_basis(woma_su_t30, observed).
narrative_ontology:measurement(woma_su_t35, woman_female_category__sex_biology_reading, suppression_requirement, 35, 0.71).
narrative_ontology:measurement_basis(woma_su_t35, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__sex_biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(woman_female_category__sex_biology_reading, 0.12).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_female_category__sex_biology_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% The 'woman/female' category kernel decomposes into three structurally distinct constraints, each representing a different reading of the category boundary. ε differs substantially across readings: the sex_biology_reading (this constraint) authors ε=0.68 for biological-category enforcement as substantially extractive (from trans people's perspective). The gender_identity_reading would author ε close to 1.0 for the same standing arrangement, treating it as pure exclusion. The hybrid_contextual_reading would author lower ε, modeling a compromise that reduces but does not eliminate extraction on both sides. All three constraints share the same kernel (the category boundary definition) and the same beneficiary/victim structure at high level, but each reading's ε value is independent and reading-indexed. They are linked by affects_constraints to signal the constraint family and institutional rivalry.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
