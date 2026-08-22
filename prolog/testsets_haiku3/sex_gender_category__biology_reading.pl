% ============================================================================
% CONSTRAINT STORY: sex_gender_category__biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__biology_reading, []).

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
 *   constraint_id: sex_gender_category__biology_reading
 *   human_readable: Sex Category Membership via Reproductive Biology (Biology Reading)
 *   domain: social/legal/identity
 *
 * SUMMARY:
 *   The biology reading of the sex/gender category kernel stipulates that
 *   membership in the categories 'man' and 'woman' is determined by immutable
 *   reproductive biology — chromosomes, gonads, and/or anatomy at birth.
 *   Under this reading, trans women are excluded from 'woman' because they
 *   lack the biological criterion; intersex individuals are forced into a
 *   binary despite not clearly meeting either category's biological
 *   definition; and cis women are the primary victim set for sex-based harms
 *   and the primary beneficiaries of sex-specific legal protections. The
 *   constraint is instantiated and enforced through state classification
 *   systems (birth certificates, legal documents), medical gatekeeping
 *   (authority to verify and certify sex), and institutional policies
 *   (sex-segregated spaces, sports, shelters). This is ONE reading of a
 *   contested kernel. The identity_reading and hybrid_reading are other
 *   constraints, not parts of this one. The biology reading claims that
 *   reproductive anatomy is the objective, immutable basis of sex category
 *   membership and that this basis should determine legal and social access.
 *   The ε value (0.68 at interval end) reflects the substantial extraction
 *   from those excluded by the biology criterion, the high suppression (0.72)
 *   required to maintain boundary enforcement against trans and intersex
 *   people's resistance, and the rising theater ratio (0.41) as enforcement
 *   machinery increasingly defends category boundaries rather than solving
 *   the founding coordination problem.
 *
 * KEY AGENTS:
 *   - cis_women: benefit from legal category affirming their reproductive anatomy; politically invested in biology reading; organize for boundary defense
 *   - trans_women: excluded from 'woman' category; identity-locked into resistance to the constraint; bear costs of exclusion and misclassification
 *   - intersex_individuals: forced into binary despite not fitting categories; powerless to resist; bear costs of forced classification
 *   - state_classification_apparatus: agenda-setter; administers biological categories through documentation; benefits from alleged clarity of biological criterion
 *   - boundary_enforcement_organizations: agenda-setters; actively defend biology reading through litigation, advocacy, policy work
 *   - medical_gatekeeping_authorities: agenda-setters; verify reproductive anatomy; control documentation and gate access to category change
 *   - human_rights_advocates: excluded; would challenge extraction and exclusion; not seated in policy conversations
 *   - feminist_disability_scholars: excluded; document harms of forced binary classification; challenge claimed objectivity of biology
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__biology_reading, 0.68).
domain_priors:suppression_score(sex_gender_category__biology_reading, 0.72).
domain_priors:theater_ratio(sex_gender_category__biology_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, accessibility_collapse, 0.79).
narrative_ontology:constraint_metric(sex_gender_category__biology_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__biology_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__biology_reading, "Sex Category Membership via Reproductive Biology (Biology Reading)").
narrative_ontology:topic_domain(sex_gender_category__biology_reading, "social/legal/identity").

domain_priors:requires_active_enforcement(sex_gender_category__biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__biology_reading, 'd3440800-44cc-4a3e-bb0f-b4d759ee3213').
narrative_ontology:cs_kernel_codification('d3440800-44cc-4a3e-bb0f-b4d759ee3213', fixed_text).
narrative_ontology:cs_authority_grounding('d3440800-44cc-4a3e-bb0f-b4d759ee3213', extraction).
narrative_ontology:cs_interpretation_layer_present('d3440800-44cc-4a3e-bb0f-b4d759ee3213').
narrative_ontology:cs_reading_relation('d3440800-44cc-4a3e-bb0f-b4d759ee3213', sex_gender_category__identity_reading, forecloses).
narrative_ontology:cs_reading_relation('d3440800-44cc-4a3e-bb0f-b4d759ee3213', sex_gender_category__hybrid_reading, influences).
narrative_ontology:cs_axiom('d3440800-44cc-4a3e-bb0f-b4d759ee3213', foundational, reproductive_anatomy_constitutes_sex).
narrative_ontology:cs_axiom_status(reproductive_anatomy_constitutes_sex, holdable).
narrative_ontology:cs_axiom_grounding('d3440800-44cc-4a3e-bb0f-b4d759ee3213', reproductive_anatomy_constitutes_sex, empirically_contingent).
narrative_ontology:cs_axiom('d3440800-44cc-4a3e-bb0f-b4d759ee3213', foundational, sex_category_immutability_from_birth).
narrative_ontology:cs_axiom_status(sex_category_immutability_from_birth, holdable).
narrative_ontology:cs_axiom_grounding('d3440800-44cc-4a3e-bb0f-b4d759ee3213', sex_category_immutability_from_birth, empirically_contingent).
narrative_ontology:cs_reference_frame('d3440800-44cc-4a3e-bb0f-b4d759ee3213', biological_determinism_administrative_simplicity).
narrative_ontology:cs_drift_state('d3440800-44cc-4a3e-bb0f-b4d759ee3213', contemporary_trans_visibility_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('d3440800-44cc-4a3e-bb0f-b4d759ee3213', '2026-06-12T14:30:00Z').
narrative_ontology:cs_kernel_id(sex_gender_category__biology_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women_category_boundary_defenders).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, state_classification_apparatus).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, trans_women).
narrative_ontology:constraint_victim(sex_gender_category__biology_reading, intersex_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, cis_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__biology_reading, boundary_enforcement_organizations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from a legal category ('woman') that affirms their reproductive anatomy as the basis of their group membership and sex-based rights claims. The biology reading vindicates their claim to exclusive representation in sex-segregated spaces and sex-specific legal protections. They experience the constraint as protective of their interests in privacy, safety, and group-coherence. Exit is theoretically available (they could endorse the identity reading) but politically costly given institutional investment in the biology frame.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, cis_women, beneficiary,
    organized, generational, mobile, global).

% Are excluded from the 'woman' category under the biology reading, despite identifying as women. They are denied access to sex-segregated spaces (bathrooms, shelters, sports, prisons) justified by the biological criterion. They bear the cost of continuous boundary verification and exclusion appeals. Their identity fusion to gender identity (not reproductive anatomy) makes exit from this constraint unthinkable — they cannot adopt the biology reading without self-negation. They absorb the constraint through institutional practices, identity denial, and social marginalization.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, trans_women, payer,
    moderate, biographical, identity_locked, global).

% Are forced into a binary classification ('male' or 'female') despite having reproductive anatomy that does not fit binary categories. The constraint offers no coherent placement for individuals with androgen insensitivity, XY karyotype with female anatomy, or ambiguous genitalia. They experience forced classification as violence and must advocate for themselves in institutional settings (medical, legal, sports) where the biology reading dominates. Exit means adoption of a false binary or constant re-litigation of category placement.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, intersex_individuals, payer,
    powerless, biographical, constrained, global).

% Administers legal categories (birth certificates, ID documents, vital records, sex-segregated services access) based on reproductive biology. The state benefits from the clarity and simplicity of the biology reading — a single, allegedly objective criterion (chromosomes, anatomy at birth) reduces classification disputes and lowers verification costs relative to subjective identity. The state uses reproductive biology as the default referent and actively enforces it through documentation requirements, medical gatekeeping for category change, and legal penalties for falsification. State institutions include medical authorities that medically verify sex.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, state_classification_apparatus, agenda_setter,
    institutional, generational, arbitrage, national).

% Organizations and movements that actively defend sex-category boundaries on biological grounds. They justify exclusionary policies (women-only spaces, sex-segregated sports, single-sex organizations) through appeals to reproductive anatomy and argue these boundaries protect women-only spaces and sex-specific rights. They benefit from cultural authority over category definitions and resources mobilized for boundary defense. They actively litigate and lobby to maintain the biology reading in policy and law.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, boundary_enforcement_organizations, agenda_setter,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__biology_reading, boundary_enforcement_organizations, beneficiary).

% Medical professionals and institutions that verify reproductive anatomy and issue certificates of biological sex. Under the biology reading, these authorities are the primary enforcers: they document sex at birth, certify anatomical status, and in many jurisdictions gate access to legal category change behind medical transition requirements. They benefit from institutional authority over classification and resources devoted to medical gatekeeping. They are positioned as neutral arbiters of biological fact.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, medical_gatekeeping_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Would object to the biology reading's exclusionary effects on trans and intersex people, citing harms from forced classification and denial of identity. They argue for recognition of gender identity and oppose mandatory biological verification. They are often excluded from policy conversations dominated by biology defenders and state institutions. Their position is not represented in the spaces where the constraint's enforcement is decided.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, human_rights_advocates, excluded,
    moderate, biographical, constrained, global).

% Document harms to intersex people from forced binary classification and question whether 'reproductive biology' is actually immutable or knowable in all cases. They are often excluded from mainstream women's advocacy spaces and policy conversations. They would argue for acknowledging both biological diversity and the institutional violence of forced classification. Their epistemic position challenges the alleged simplicity and objectivity of the biology reading.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, feminist_disability_scholars, excluded,
    moderate, biographical, constrained, global).

% Observes the constraint from outside all seats. Tracks the structural relationships, enforcement costs, and excluded voices. Notes that the biology reading's claim to objectivity and immutability depends on institutional verification practices, not on biology itself as self-evident.
narrative_ontology:constraint_stakeholder(sex_gender_category__biology_reading, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__biology_reading, state_classification_apparatus).
narrative_ontology:fixing_cost_class(sex_gender_category__biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a stable, legally recognized classification system for distributing access to sex-segregated spaces (bathrooms, shelters, sports, prisons) and sex-specific legal protections (reproductive rights, anti-discrimination law, harassment redress). The coordination problem: how to allocate these goods and protections consistently without requiring individual negotiation in every context. The biology reading solves this by stipulating a single, allegedly objective criterion (reproductive anatomy at birth) that requires minimal institutional cost to verify.
% TRANSFER_FUNCTION: Transfers social authority, institutional recognition, and legal access rights FROM those excluded by the biology criterion (trans women, intersex individuals, some non-binary people) TO cis women and to state classification institutions. More abstractly: moves the authority to define category membership FROM gender identity and lived experience TO reproductive anatomy and state medical verification. The constraint also transfers resources devoted to boundary enforcement and verification — medical gatekeeping, legal documentation, institutional audit, and exclusion appeals consume time and institutional capacity.
% ABSENT_VOICES: Trans women are formally excluded from women-only spaces and organizations, and structurally excluded from policy conversations about sex category definitions dominated by state institutions and boundary-defending organizations. Intersex individuals are nearly entirely absent from these conversations. Disability scholars and intersex advocates who challenge the claim that reproductive biology is simple or immutable are systematically marginalized. Their objections would complicate the alleged objectivity of the biology reading and raise verification costs.
% DISAPPEARANCE_RATIONALE: If the biology reading vanished overnight — if reproductive anatomy at birth ceased to be the legal criterion for sex category membership — institutions would immediately face classification questions: How would people be categorized in legal documents? What would access to sex-segregated spaces be based on? Legal disputes would proliferate; some jurisdictions would adopt identity-based or hybrid criteria; institutional practices would shift. The allocation of sex-segregated goods and the distribution of sex-specific legal protections would become contested in every context. The constraint is not a natural fact; its disappearance would require rapid institutional renegotiation.
% FOUNDING_PROBLEM: Pre-modern legal systems lacked a standardized category for allocating sex-segregated goods and enforcing reproductive and family law. Modern nation-states developed sex classification to create administrative order: a single criterion (reproductive biology) could be documented at birth and used consistently in law. The problem was institutional complexity without a clear rule for sex assignment.
% FOUNDING_PROBLEM_CORROBORATION: State institutions and medical authorities attest the founding problem is live and that biological classification remains the clearest administrative criterion. Boundary-defending organizations align with this reading. However, scholars of gender history, trans advocates, and intersex activists contest this claim: they argue the founding problem is substantially solved (sex classification is now routine for most people), that the biology reading persists not because it solves an active problem but because institutional investment in it is high and alternatives are politically costly, and that the founding problem's 'solution' has produced a new harms problem (exclusion and misclassification of trans and intersex people). No corroboration from outside the benefiting parties (state institutions and boundary defenders) affirms the problem-is-live reading. Disability scholars and trans scholars produce detailed critiques from outside the beneficiary set.
narrative_ontology:disappearance_verdict(sex_gender_category__biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(sex_gender_category__biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__biology_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sex_gender_category__biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sex_gender_category__biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68) reflects asymmetric harm: trans women and intersex individuals bear substantial costs (exclusion from spaces, legal non-recognition, forced misclassification) while cis women and state institutions collect benefits (institutional clarity, legal authority, resource allocation by biology). Suppression (0.72) is high because the constraint's persistence depends on actively excluding trans women from spaces where they would otherwise be admitted and preventing intersex people from challenging binary categories. The extraction is not incidental; enforcement machinery exists specifically to maintain these exclusions. Theater ratio (0.41, rising over time) indicates that enforcement increasingly functions as boundary defense rather than solving the foundational coordination problem (allocating sex-segregated goods). Early in the interval, enforcement aimed at verifying sex for legitimate institutional purposes; by interval end, enforcement is substantially directed at excluding trans people who challenge the biology reading's legitimacy. The measurement series shows extraction, suppression, and theater all rising as resistance to the biology reading grows: each rise in trans visibility and intersex advocacy triggers institutional defensive responses (stricter verification, higher enforcement costs, more theatrical appeals to 'objective biology'). All metrics share one time grid; no metric missing from any time point.
 *
 * PERSPECTIVAL GAP:
 *   The cis-women and state-institution seats should compute very differently from the trans-women and intersex seats. From the cis-women perspective (high power, organized, mobile exit, beneficiary role), the constraint is protective coordination — a clear rule that affirms their group membership and sex-specific rights. From the trans-women perspective (moderate power, identity-locked exit, payer role), the same constraint is extractive exclusion — a rule that denies their self-identified category membership and confines them to excluded spaces. The engine computes this divergence from the structural data: d values differ dramatically across seats because beneficiary/victim status differs, exit options differ (mobile vs. identity-locked), and power levels differ. The biology reading's claimed objectivity masks this perspectival divergence; the divergence in computed types reveals that the reading produces fundamentally different constraint experiences for different seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Cis women: beneficiary role, organized power, mobile exit (they could endorse the identity reading but choose not to) → d near 0.0 (subsidized by the constraint). Trans women: payer role, moderate power, identity-locked exit (they cannot exit into the biology reading without self-negation) → d near 1.0 (extracted from). Intersex individuals: payer role, powerless, constrained exit (they must adopt a false binary) → d = 1.0 (fully targeted). State apparatus: agenda-setter role, institutional power, arbitrage exit (could change the rule but chooses to maintain biology) → d near beneficiary end (captures classification authority). Boundary-defending organizations: agenda-setter role, organized power, mobile exit → d near 0.0 (subsidized by the constraint's authority over women-only space definitions). Medical gatekeeping authorities: agenda-setter role, institutional power, arbitrage exit → d near beneficiary end (controls gatekeeping and documentation). The directionality divergence is structural: the same rule produces wildly different d values across seats because beneficiary/victim and power/exit differ sharply.
 *
 * MANDATROPHY ANALYSIS:
 *   The biology reading faces the mandatrophy structure: it was founded to solve the administrative problem of standardizing sex classification for institutional purposes (founding problem: pre-modern legal chaos). That problem is arguably solved — modern states do routinely classify people by reproductive biology and issue consistent legal documents. However, the constraint persists and strengthens (extraction and suppression rise over the interval) despite the founding problem's partial resolution. The constraint now functions primarily as boundary defense against the identity reading — the extraction serves to maintain the category-membership rule against trans challenges, not to solve the founding administrative problem. The theater ratio rise (from 0.28 to 0.41) shows enforcement increasingly directed at exclusion theater: appeals to 'objective biology,' regulatory tightening around sex verification, institutional defenses against trans inclusion. The constraint exhibits mandatrophy: the founding coordination function is solved, the extraction persists, and the persistence is now explained by institutional inertia and the political investment of benefiting parties (cis women, boundary defenders, state institutions) rather than by ongoing coordination necessity. The engine should flag this via the mandatrophy_analysis and the contest between founding_problem_status=contested and disappearance_verdict=world_rearranges.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    biological_essentialism_contingency,
    'Is the claim that reproductive biology is the objective, immutable basis of sex category membership a fact of nature, or a social choice about which biological facts to emphasize for institutional purposes?',
    'Comparative institutional analysis: survey legal systems that use different criteria (identity, hybrid, biology) and evaluate the coherence and simplicity of each criterion in practice. Historical analysis: trace how the biology criterion became dominant in modern law (not inevitable, but a contingent institutional choice). Scientific analysis: examine whether biology itself is as binary and immutable as the reading claims (intersex variation, XY/XX ambiguity, epigenetics, hormone variation suggest complexity).',
    'If biology is revealed as a social choice rather than a natural fact, the reading''s claim to objectivity collapses. The constraint would be reclassified from an allegedly natural boundary to an actively constructed and enforced extraction mechanism. This would reshape the mandatrophy analysis: the constraint persists not because of biological reality but because institutional actors benefit from the clarity and authority the biology reading provides.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(biological_essentialism_contingency, conceptual, 'Whether ''reproductive biology'' is natural fact or institutionalized choice.').

omega_variable(
    verification_cost_reality,
    'What is the actual institutional cost of verifying reproductive biology (at birth and ongoing) relative to the cost of verifying gender identity or a hybrid criterion?',
    'Audit of state documentation systems and medical gatekeeping institutions to measure actual costs of sex verification (genetic testing, ultrasound, birth recording, ongoing documentation updates). Compare to costs of identity verification (self-declaration plus safeguards, lived experience documentation) or hybrid verification (medical transition plus recognition). Jurisdiction comparison: measure implementation costs in places using different criteria.',
    'If biology verification is not actually simpler or cheaper than identity or hybrid verification, the reading''s justification (that biology is a clear, low-cost criterion) collapses. The extraction would be revealed as not grounded in efficiency but in institutional power and boundary-defending interests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_reality, empirical, 'Whether the biology reading actually minimizes classification costs or merely appears to.').

omega_variable(
    intersex_category_coherence,
    'Is it possible to apply the biology reading coherently to intersex individuals, or does intersex variation force the reading into incoherence?',
    'Legal and medical case analysis: examine how states handle intersex individuals under biology-based classification (forced binary assignment, ambiguous anatomy cases, androgen insensitivity, XY with female anatomy). Interview intersex people and medical authorities about classification experiences. Analyze whether the constraint can accommodate intersex without modification or whether it inevitably forces violence (forced binary assignment, erasure, repeated re-litigation).',
    'If the biology reading is incoherent for intersex people, the reading''s claim to be based on an objective, universally applicable biological criterion fails. The constraint would be revealed as working only for the non-intersex majority and as inflicting forced binary assignment on a substantial minority. This strengthens the mandatrophy case: the constraint persists to maintain cis-woman boundaries at the cost of intersex erasure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intersex_category_coherence, empirical, 'Whether the biology criterion can coherently classify intersex individuals or forces incoherence and erasure.').

omega_variable(
    identity_locked_vs_structural_exit_ambiguity,
    'For trans women, how much of the suppression is structural (external legal barriers, institutional exclusion policies) versus internalized (absorbed belief that the reading is objective truth, identity fusion to the excluded category)?',
    'Post-exit analysis: in jurisdictions that legalize identity-based category recognition, measure whether trans women''s suppression persists after legal barriers are removed. Document lived experience: interview trans women about whether exclusion feels externally imposed or psychologically internalized. Measure institutional costs: assess whether ''post-exit'' (after legal recognition in a more-inclusive jurisdiction) trans women experience the constraint as gone or as persisting in altered form.',
    'If suppression is substantially internalized, the constraint''s effective suppression is higher than the structural measure suggests — trans women carry the internalization even after external barriers are removed. This would reshape the classification: the constraint would appear less suppressively severe when measured by external barriers alone, but substantially more severe when internalization is accounted for. It would also illuminate the mechanism by which a reading''s claim to objectivity (the reading''s framing as ''just biology'') produces internalized assent even in those it harms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_vs_structural_exit_ambiguity, empirical, 'Whether trans suppression is primarily structural or internalized, and how much persists post-exit.').

omega_variable(
    foundation_problem_alternative_readings,
    'If the founding problem is now solved (sex classification is routine), why does the biology reading persist and strengthen rather than being replaced by whichever reading most efficiently allocates sex-segregated goods and sex-specific rights?',
    'Path-dependency analysis: document when and how the biology reading became institutionalized (when did birth certificates standardize to biological sex; when did legal category change become gatekept by medical authorities; when did boundary-defending organizations become powerful). Counterfactual: examine alternative readings that emerged later (identity, hybrid) and ask why they were not adopted if the biology reading were merely instrumental. Political economy: analyze the interests of institutional actors in maintaining biology versus adopting alternatives.',
    'If the biology reading persists despite efficiency no longer requiring it, the constraint''s classification is mandatrophy: extraction without present coordination necessity. The founding problem (pre-modern legal chaos) is solved; the constraint now functions primarily as boundary defense and institutional authority maintenance for benefiting parties. This would support the mandatrophy analysis and strengthen the case for the constraint as Tangled Rope or Snare rather than Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foundation_problem_alternative_readings, conceptual, 'Why the biology reading persists when its founding problem appears solved.').

omega_variable(
    reading_frame_contest_ambiguity,
    'Is the contest between the biology reading and identity reading fundamentally about different understandings of what ''real sex'' or ''real gender'' is (committer-axis disagreement), or about distributive interests in who gets to be called ''woman'' and access women-only goods?',
    'Textual analysis of arguments for each reading: examine whether proponents primarily argue for the reading''s epistemological truth (what sex/gender really is) or its policy consequences (who would benefit if this reading became law). Interview advocates from each reading: ask whether they would support the other reading if its policy consequences were equivalent to the current reading''s. Institutional analysis: track which interests benefit from each reading and whether benefits predict reading adoption across jurisdictions.',
    'If the contest is primarily about distributional interests rather than epistemic truth, the reading''s claim to objectivity is revealed as cover for a distributional fight. The constraint would be classified as pure extraction dressed in objectivity-claims rather than as genuine disagreement about what sex/gender is. This would strengthen the Snare classification and weaken the Tangled Rope classification (if it is genuinely hybrid coordination + extraction, the coordination story must be separable from the extraction; if both are cover, it is pure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_frame_contest_ambiguity, conceptual, 'Whether the reading contest is epistemological or distributional.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__biology_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__biology_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement(sex__tr_t8, sex_gender_category__biology_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement(sex__tr_t16, sex_gender_category__biology_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__biology_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement(sex__tr_t35, sex_gender_category__biology_reading, theater_ratio, 35, 0.41).
narrative_ontology:measurement(sex__tr_t50, sex_gender_category__biology_reading, theater_ratio, 50, 0.41).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__biology_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sex__be_t8, sex_gender_category__biology_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement(sex__be_t16, sex_gender_category__biology_reading, base_extractiveness, 16, 0.61).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__biology_reading, base_extractiveness, 25, 0.65).
narrative_ontology:measurement(sex__be_t35, sex_gender_category__biology_reading, base_extractiveness, 35, 0.67).
narrative_ontology:measurement(sex__be_t50, sex_gender_category__biology_reading, base_extractiveness, 50, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__biology_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(sex__su_t8, sex_gender_category__biology_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement(sex__su_t16, sex_gender_category__biology_reading, suppression_requirement, 16, 0.66).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__biology_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement(sex__su_t35, sex_gender_category__biology_reading, suppression_requirement, 35, 0.72).
narrative_ontology:measurement(sex__su_t50, sex_gender_category__biology_reading, suppression_requirement, 50, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__biology_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(sex_gender_category__biology_reading, 0.12).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__identity_reading).
narrative_ontology:affects_constraint(sex_gender_category__biology_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The sex/gender category kernel decomposes into three constraints, each representing a different reading. The biology_reading (this constraint) stipulates reproductive anatomy as the category criterion. The identity_reading stipulates gender identity as the criterion. The hybrid_reading stipulates medical transition plus social recognition. Each reading has distinct beneficiary/victim structures, distinct enforcement mechanisms, and distinct ε values. The three constraints are linked by network.affects_constraints because they are alternative readings of the same institutional commitment (sex category membership) and because policy choices to adopt one reading affect the institutional conditions under which the other readings operate. The biology reading is upstream: it is currently institutionalized in most jurisdictions, and policy moves to identity or hybrid criteria represent downstream shifts away from biology. The identity and hybrid readings influence the biology reading's persistence (as their alternatives become more politically salient, the biology reading must be actively defended rather than passively maintained).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__biology_reading, moderate, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
