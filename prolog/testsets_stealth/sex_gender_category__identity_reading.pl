% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sex_gender_category__identity_reading, []).

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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Self-Identification Rule for Gender Category Membership
 *   domain: social ontology/legal classification
 *
 * SUMMARY:
 *   This story instantiates the identity_reading of the sex_gender_category
 *   kernel: the rule that category membership ('woman', 'man') is determined
 *   by first-person identification rather than by reproductive biology or by
 *   medically certified transition. Enacted first in Argentina (2012) and
 *   since adopted in varying forms across Europe, North America, and Oceania,
 *   the rule replaces gatekeeping with declaration. Its operation coordinates
 *   (a single administrable membership rule, dignity of recognition, removal
 *   of medical gatekeeping) while simultaneously transferring something: cis
 *   women's exclusive claim to sex-based protections dissolves, the guarantee
 *   behind 'women-only' provisions becomes contestable setting by setting,
 *   and dissent from the rule carries professional, platform, and sometimes
 *   legal sanction. The expected structural delta holds: trans women enter
 *   the category and its exposures (including misogyny), the payer set
 *   expands to cis women who relied on sex-based provisions, boundary
 *   enforcement is administratively cheap, and the conflict concentrates on
 *   space access. KEY AGENTS (by structural relationship): - trans_women:
 *   primary beneficiary (organized/identity_locked) — gains membership by
 *   declaration, becomes subject to misogyny as a woman - trans_men: parallel
 *   beneficiary (moderate/identity_locked) — low-friction recognition -
 *   cis_women_protection_claimants: primary payer (organized/constrained) —
 *   dissolved exclusivity of sex-based provision - incarcerated_women:
 *   concentrated payer (powerless/trapped) — highest-stakes space-access
 *   exposure - female_contact_athletes: payer (moderate/constrained) —
 *   changed competitive field, split governance response -
 *   selfid_statute_legislatures and equality_tribunals_and_commissions:
 *   agenda-setters (institutional) — set and administer the rule -
 *   gender_critical_feminists: payer and excluded voice
 *   (organized/identity_locked) — organized resistance bearing sanction costs
 *   - service_providers_and_employers: dual payer/beneficiary
 *   (institutional/constrained) - official_statistics_bodies: payer
 *   (institutional/generational) — measurement-integrity costs. The claim and
 *   the metrics are independent authored facts: claimed_type states what I
 *   believe is structurally true; the metric values state what I believe is
 *   descriptively true of the rule's actual operation.
 *
 * KEY AGENTS:
 *   - trans_women: primary beneficiary (organized/identity_locked) — membership by declaration; newly exposed to misogyny as category members
 *   - trans_men: secondary beneficiary (moderate/identity_locked) — parallel recognition with low public friction
 *   - cis_women_protection_claimants: primary payer (organized/constrained) — lose exclusive claim to sex-based protections
 *   - incarcerated_women: concentrated payer (powerless/trapped) — custodial space allocation decided over their heads
 *   - female_contact_athletes: payer (moderate/constrained) — category boundary moved under an athletic career
 *   - selfid_statute_legislatures: agenda-setter (institutional/constrained) — enacted the rule; amendment now politically costly
 *   - equality_tribunals_and_commissions: agenda-setter (institutional/constrained) — daily administration and enforcement
 *   - gender_critical_feminists: payer and excluded voice (organized/identity_locked) — organized resistance facing sanction
 *   - service_providers_and_employers: dual payer/beneficiary (institutional/constrained) — simplified administration, bidirectional litigation exposure
 *   - official_statistics_bodies: payer (institutional/generational) — longitudinal data integrity costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.38).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.57).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.57).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Self-Identification Rule for Gender Category Membership").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social ontology/legal classification").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '70a6ede6-441a-41fe-9cbe-a450418ccabb').
narrative_ontology:cs_kernel_codification('70a6ede6-441a-41fe-9cbe-a450418ccabb', distributed).
narrative_ontology:cs_authority_grounding('70a6ede6-441a-41fe-9cbe-a450418ccabb', self_enforcing).
narrative_ontology:cs_reading_relation('70a6ede6-441a-41fe-9cbe-a450418ccabb', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('70a6ede6-441a-41fe-9cbe-a450418ccabb', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('70a6ede6-441a-41fe-9cbe-a450418ccabb', foundational, first_person_identity_authoritative_for_category).
narrative_ontology:cs_axiom_status(first_person_identity_authoritative_for_category, holdable).
narrative_ontology:cs_axiom_grounding('70a6ede6-441a-41fe-9cbe-a450418ccabb', first_person_identity_authoritative_for_category, deontological).
narrative_ontology:cs_axiom('70a6ede6-441a-41fe-9cbe-a450418ccabb', secondary, medical_gatekeeping_invalid_as_membership_price).
narrative_ontology:cs_axiom_status(medical_gatekeeping_invalid_as_membership_price, holdable).
narrative_ontology:cs_axiom_grounding('70a6ede6-441a-41fe-9cbe-a450418ccabb', medical_gatekeeping_invalid_as_membership_price, deontological).
narrative_ontology:cs_reference_frame('70a6ede6-441a-41fe-9cbe-a450418ccabb', self_defined_gender_membership).
narrative_ontology:cs_drift_state('70a6ede6-441a-41fe-9cbe-a450418ccabb', contemporary_backlash_period, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('70a6ede6-441a-41fe-9cbe-a450418ccabb', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_men).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women_protection_claimants).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, incarcerated_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, female_contact_athletes).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, gender_critical_feminists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, service_providers_and_employers).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, service_providers_and_employers).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, official_statistics_bodies).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, self_identification_principle).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, yogyakarta_self_definition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live as women and seek legal and social recognition consistent with that identity. Under this rule they obtain category membership by declaration, with no psychiatric diagnosis, hormone requirement, or panel review standing between them and their documents, names, and access. They become subject to the protections directed at women and also to the hostilities, including misogynistic violence. The category question is not one they can step outside of: the claim that their identity determines their membership is constitutive of how they live.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    organized, biographical, identity_locked, global).

% Occupy the parallel position on the other side of the category line, obtaining membership in 'man' by declaration. Their recognition proceeds with far less public friction than the corresponding case for trans women, and their access gains in documentation and men's facilities attract little organized opposition.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_men, beneficiary,
    moderate, biographical, identity_locked, global).

% Rely on sex-based provisions such as refuges, hospital wards, rape crisis services, and changing rooms on the understanding that 'women-only' tracks sex. Under this rule the category they are promised expands by declaration, and the guarantee they understood themselves to hold becomes contestable in each new setting. They cannot exit being categorized; their recourse is to argue for exceptions service by service, which marks them publicly and carries social and professional cost.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women_protection_claimants, payer,
    organized, biographical, constrained, global).

% Are held in custodial settings where housing allocation, search protocols, and cell assignments follow the category rule. They have no exit at all from the assignment decision and minimal voice in the policy that sets it. This seat concentrates the highest-stakes version of the conflict over space access.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, incarcerated_women, payer,
    powerless, biographical, trapped, national).

% Compete in women's categories in sports where strength, speed, or body contact materially shape outcomes. Where the category boundary follows identity, the competitive field they trained for changes; several international federations have responded by re-anchoring eligibility in biology, leaving athletes governed by inconsistent rules depending on the sport and jurisdiction.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, female_contact_athletes, payer,
    moderate, biographical, constrained, global).

% Enacted the rule as statute or official guidance, beginning with Argentina in 2012 and followed by Ireland, Malta, Denmark, and parts of Canada, Australia, and the United States. They retain formal authority to amend the rule in either direction, but amendment now carries heavy political cost on both sides and, in some jurisdictions, exposure under international human-rights obligations that require some route to legal recognition.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, selfid_statute_legislatures, agenda_setter,
    institutional, generational, constrained, national).

% Administer the rule day to day: hear complaints about misgendering and exclusion, issue guidance to employers and service providers, and police the line between the rule and its claimed exceptions. Their dockets and guidance documents are where the rule's enforcement actually happens, and their interpretations move its practical reach without any statutory change.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, equality_tribunals_and_commissions, agenda_setter,
    institutional, generational, constrained, national).

% Hold that women's protections presuppose a sex-based boundary and organize to contest the rule through litigation, consultation responses, and public argument. They face employment consequences, platform removals, and no-platforming, and report being shut out of the consultations where the rule was drafted. Their position is bound up with their broader political identity; withdrawing from the dispute would mean abandoning commitments they regard as definitional of their feminism.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_critical_feminists, payer,
    organized, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, gender_critical_feminists, excluded).

% Operate facilities, records, and rosters under whatever determination rule their jurisdiction sets. The rule simplifies administration by removing any verification burden, but exposes them to complaint from both directions: exclusion grievances if they gate access by sex, and user objections if they do not. Multinationals tend to adopt the most permissive common denominator across their operations.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, service_providers_and_employers, payer,
    institutional, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, service_providers_and_employers, beneficiary).

% Maintain longitudinal series on health, crime, pay gaps, and demographics keyed to the category. When the category's composition shifts by declaration, comparability breaks, and each census or survey cycle forces a choice between asking about identity, about sex, or both, with every available choice drawing criticism from one side or the other.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, official_statistics_bodies, payer,
    institutional, generational, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, trans_women).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single administrable rule for who counts as a woman or man across documents, facilities, rosters, and anti-discrimination coverage, solved by first-person declaration instead of medical panels, diagnosis requirements, or bodily verification.
% TRANSFER_FUNCTION: Moves exclusive claim to sex-based category membership and the protections attached to it from cis women as a sex-defined class to everyone who identifies into the category; moves recognition authority from medical institutions to individuals; and moves compliance and sanction risk onto dissenters, service providers, and statistics bodies.
% ABSENT_VOICES: Gender-critical feminists report exclusion from the consultations where self-ID was drafted, and their consultation submissions were contested in several jurisdictions; incarcerated women and frontline service users are rarely consulted at all; detransitioners and gender-nonconforming people skeptical of affirmation-only pathways have no institutional seat.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, trans people would fall back on medical-gatekeeping regimes where they still exist, institutions would re-impose biological or diagnostic tests for category membership, and the entire conflict would rearrange around gatekeeping criteria rather than dissolving. Documents, facility policies, sports eligibility, and statistics collection would all reorganize.
% FOUNDING_PROBLEM: Trans people faced medicalized, expensive, and humiliating gatekeeping to change legal sex, including psychiatric diagnosis requirements and, historically, sterilization requirements in some jurisdictions, while living with documents that mismatched their identity exposed them to danger and indignity in ordinary transactions.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: European Court of Human Rights jurisprudence predating any self-ID statute (Goodwin v UK, 2002) attests the reality and severity of the gatekeeping burden; parliamentary findings in Ireland and Malta document it independently. Opponents of the rule attest the founding problem was real while disputing whether self-identification is the right remedy, which corroborates the problem's existence separately from this reading's preferred solution.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.38, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sex_gender_category__identity_reading_tests).
:- end_tests(sex_gender_category__identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.38 at interval end): the rule's transfer is real but bounded — exclusive claims and guarantees dissolve for cis women in specific high-stakes settings (custody, intimate services, sport, data) while most everyday contexts are unaffected, and the gains to trans category members are large. Suppression (0.57) reflects the enforcement machinery the rule requires: misgendering and exclusion complaints, employment and platform consequences for dissent, and guidance that narrows claimed exceptions; it is substantial but visibly contested rather than total. Theater ratio (0.28) captures performative compliance — pronoun declarations, institutional statements — layered on a genuinely functioning recognition rule, easing slightly as some institutions quietly retrench during the rollback period. Accessibility collapse (0.60): within the reading's own framework, accepting the identity premise collapses bio-based alternatives almost completely (any sex-gated exclusion becomes definitionally suspect), but across jurisdictions the sibling readings remain live law, so societal-level alternatives persist. Resistance (0.75) is among the highest of any contemporary social constraint: litigation, legislation, federation rule changes, and mass political mobilization on both sides. The temporal series share one grid: extractiveness climbs as the rule extends from documents into services, sport, and data (t0-t12), then dips slightly as rollback decisions trim its reach; suppression_requirement peaks around t10 as enforcement infrastructure matures, then eases as contested jurisdictions retreat; theater rises with compliance culture and recedes with retrenchment. The claimed type (tangled_rope) reflects my structural judgment: a genuine coordination function (administrable, dignified membership determination) with asymmetric transfer through the same structure (payers named above) held in place by active enforcement — not pure coordination, since identifiable groups bear real costs, and not pure extraction, since the coordination function is real and the founding problem remains live.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the trans_women seat the rule is close to pure subsidy: it delivers recognition, safety, and dignity at near-zero enforcement cost to them, and their identity_locked position means no exit exists or is wanted. From the incarcerated_women seat the same rule arrives as an unappealable allocation decision made by others, with no exit at all — the fullest-target position in the story. The legislature seat experiences the rule as a settled achievement it built and now cannot cheaply touch; the tribunal seat experiences it as a growing docket of hard cases; the statistics seat experiences it as slow data corruption. Service providers sit genuinely dual: administration got simpler (benefit) while litigation exposure grew (cost). These divergences are computed by the engine from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (trans_women, trans_men) derive low directionality — the rule subsidizes them, amplified by identity_lock, which stabilizes their position at the beneficiary end rather than merely placing them there. Payers derive high directionality, with trapping and lock amplifying it: incarcerated_women (powerless, trapped) sit nearest the full-target end; cis_women_protection_claimants (constrained) somewhat below; gender_critical_feminists combine target position with identity_lock, meaning their opposition is constitutive rather than strategic. Service providers' dual role splits their effective position near symmetric. National-to-global scopes scale effective extraction modestly upward for the payer seats, since verification of exceptions is harder at larger scope. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct relationships for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — medicalized gatekeeping exposing trans people to danger and indignity — remains live wherever the rule has not been adopted, and even where adopted, recognition questions continue to generate casework. founding_problem_status is therefore live, and the (status x disappearance_verdict) pair reads live x world_rearranges: no capture-or-zombie flag fires, and mandatrophy_resolved is deliberately not declared. The classification work here prevents two opposite mislabels. Reading the rule as pure coordination (rope) would erase the named payers — the women whose exclusive claims dissolved and the prisoners whose housing is decided over their heads — and would treat the enforcement machinery as overhead rather than as load-bearing. Reading it as pure extraction (snare) would erase the real coordination function: some determination rule for the category must exist, the medical-gatekeeping alternative imposed heavier costs on a vulnerable population, and the rule's administrative simplicity is a genuine benefit to institutions. Tangled rope holds both halves: coordination through the front, transfer through the same door, enforcement keeping the door shaped as it is.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint instantiates the identity_reading of the sex_gender_category kernel; how would adopting the biology_reading or the hybrid_reading change the structural picture?',
    'Compare the three sibling stories directly: biology_reading shrinks the category, returning trans women to the victim set of denial and restoring cis women''s exclusive claims; hybrid_reading reinstates medical gatekeeping as the price of membership, moving gatekeeping authority back to clinical institutions.',
    'Every classification output here is reading-relative: swapping readings swaps the entire beneficiary/victim structure and the epsilon. Cross-reading comparisons that ignore the swap produce spurious verdicts about which arrangement is more extractive.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer structure: one of three mutually exclusive readings of a contested category-membership kernel.').

omega_variable(
    space_access_conflict_settlement,
    'Will the conflict over space access settle through inclusive provision plus third spaces and targeted exceptions, or persist as a permanent front-line dispute?',
    'Longitudinal service-user surveys and incident data from self-ID jurisdictions, tracking whether refuge, ward, and facility disputes decline as accommodation norms mature.',
    'Persistent conflict keeps effective extraction elevated and enforcement load high for the payer seats; genuine settlement would lower both and soften the incarcerated_women and cis_women_protection_claimant positions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(space_access_conflict_settlement, empirical, 'Whether the space-access conflict is transitional friction or a structural feature of the rule.').

omega_variable(
    sports_carveout_stability,
    'Will the international federations'' biology-anchored eligibility rules stabilize as accepted exceptions to the identity rule, or be harmonized back into it through litigation and politics?',
    'Track federation policy cycles and CAS/court challenges over the coming decade; stability of the carve-outs across jurisdictions indicates acceptance.',
    'Stable carve-outs shrink the female_contact_athletes payer seat and reduce measured extraction; harmonization expands it and pushes the rule''s extractiveness upward.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sports_carveout_stability, empirical, 'Whether sport becomes a durable exception domain or rejoins the unified rule.').

omega_variable(
    suppression_internalization_ambiguity,
    'Is the measured suppression of dissent structural (employment, platform, and legal sanctions) or partly internalized (self-censorship and anticipatory conformity that persists absent sanctions)?',
    'Post-sanction-removal speech trajectories: compare expression rates in jurisdictions that narrowed misgendering codes or shielded belief (e.g., post-For Women Scotland clarifications) against those that did not.',
    'If substantially internalized, effective suppression exceeds the structural measure and outlives enforcement changes; the constraint would resist liberalization more than its written form suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_internalization_ambiguity, empirical, 'Structural versus internalized components of dissent suppression.').

omega_variable(
    category_indexicality_framing,
    'Is this constraint best framed as the determination rule itself (who counts), or as the legitimacy claim layered above it (that first-person reports are authoritative over bodily facts)? The two framings are both coherent and yield different commitment-system classifications.',
    'Test which framing better predicts institutional behavior across jurisdictions: if tribunals and legislatures treat the rule as revisable policy, the rule-framing dominates; if they treat challenge to it as heresy rather than error, the legitimacy-claim framing dominates.',
    'The rule-framing yields a conventional, amendable arrangement; the legitimacy-claim framing yields a commitment-system structure with axiom-level defense behavior and different foreclosure dynamics against the sibling readings.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(category_indexicality_framing, conceptual, 'Framing under-determination: determination rule versus authoritative-legitimacy claim.').

omega_variable(
    rollback_expansion_trajectory,
    'Does the contemporary repudiation wave (the UK Supreme Court''s 2025 For Women Scotland ruling, United States federal policy shifts, federation eligibility rules) spread to further jurisdictions, or stall while self-ID consolidates elsewhere?',
    'Comparative tracking of statutes, case law, and administrative guidance across jurisdictions through roughly 2032.',
    'Resolves the drift_state direction: sustained rollback deepens repudiation pressure toward severe; renewed expansion returns the reading''s reference frame toward stable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(rollback_expansion_trajectory, empirical, 'Trajectory of the current rollback wave against continued expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 14).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgc_identity_read_tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sgc_identity_read_tr_t0, observed).
narrative_ontology:measurement(sgc_identity_read_tr_t2, sex_gender_category__identity_reading, theater_ratio, 2, 0.18).
narrative_ontology:measurement_basis(sgc_identity_read_tr_t2, observed).
narrative_ontology:measurement(sgc_identity_read_tr_t4, sex_gender_category__identity_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement_basis(sgc_identity_read_tr_t4, observed).
narrative_ontology:measurement(sgc_identity_read_tr_t6, sex_gender_category__identity_reading, theater_ratio, 6, 0.26).
narrative_ontology:measurement_basis(sgc_identity_read_tr_t6, observed).
narrative_ontology:measurement(sgc_identity_read_tr_t8, sex_gender_category__identity_reading, theater_ratio, 8, 0.29).
narrative_ontology:measurement_basis(sgc_identity_read_tr_t8, observed).
narrative_ontology:measurement(sgc_identity_read_tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(sgc_identity_read_tr_t10, observed).
narrative_ontology:measurement(sgc_identity_read_tr_t12, sex_gender_category__identity_reading, theater_ratio, 12, 0.3).
narrative_ontology:measurement_basis(sgc_identity_read_tr_t12, observed).
narrative_ontology:measurement(sgc_identity_read_tr_t14, sex_gender_category__identity_reading, theater_ratio, 14, 0.28).
narrative_ontology:measurement_basis(sgc_identity_read_tr_t14, observed).

% Extraction over time
narrative_ontology:measurement(sgc_identity_read_be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(sgc_identity_read_be_t0, observed).
narrative_ontology:measurement(sgc_identity_read_be_t2, sex_gender_category__identity_reading, base_extractiveness, 2, 0.32).
narrative_ontology:measurement_basis(sgc_identity_read_be_t2, observed).
narrative_ontology:measurement(sgc_identity_read_be_t4, sex_gender_category__identity_reading, base_extractiveness, 4, 0.34).
narrative_ontology:measurement_basis(sgc_identity_read_be_t4, observed).
narrative_ontology:measurement(sgc_identity_read_be_t6, sex_gender_category__identity_reading, base_extractiveness, 6, 0.36).
narrative_ontology:measurement_basis(sgc_identity_read_be_t6, observed).
narrative_ontology:measurement(sgc_identity_read_be_t8, sex_gender_category__identity_reading, base_extractiveness, 8, 0.37).
narrative_ontology:measurement_basis(sgc_identity_read_be_t8, observed).
narrative_ontology:measurement(sgc_identity_read_be_t10, sex_gender_category__identity_reading, base_extractiveness, 10, 0.38).
narrative_ontology:measurement_basis(sgc_identity_read_be_t10, observed).
narrative_ontology:measurement(sgc_identity_read_be_t12, sex_gender_category__identity_reading, base_extractiveness, 12, 0.39).
narrative_ontology:measurement_basis(sgc_identity_read_be_t12, observed).
narrative_ontology:measurement(sgc_identity_read_be_t14, sex_gender_category__identity_reading, base_extractiveness, 14, 0.38).
narrative_ontology:measurement_basis(sgc_identity_read_be_t14, observed).

% Suppression requirement over time
narrative_ontology:measurement(sgc_identity_read_su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.45).
narrative_ontology:measurement_basis(sgc_identity_read_su_t0, observed).
narrative_ontology:measurement(sgc_identity_read_su_t2, sex_gender_category__identity_reading, suppression_requirement, 2, 0.5).
narrative_ontology:measurement_basis(sgc_identity_read_su_t2, observed).
narrative_ontology:measurement(sgc_identity_read_su_t4, sex_gender_category__identity_reading, suppression_requirement, 4, 0.55).
narrative_ontology:measurement_basis(sgc_identity_read_su_t4, observed).
narrative_ontology:measurement(sgc_identity_read_su_t6, sex_gender_category__identity_reading, suppression_requirement, 6, 0.58).
narrative_ontology:measurement_basis(sgc_identity_read_su_t6, observed).
narrative_ontology:measurement(sgc_identity_read_su_t8, sex_gender_category__identity_reading, suppression_requirement, 8, 0.61).
narrative_ontology:measurement_basis(sgc_identity_read_su_t8, observed).
narrative_ontology:measurement(sgc_identity_read_su_t10, sex_gender_category__identity_reading, suppression_requirement, 10, 0.63).
narrative_ontology:measurement_basis(sgc_identity_read_su_t10, observed).
narrative_ontology:measurement(sgc_identity_read_su_t12, sex_gender_category__identity_reading, suppression_requirement, 12, 0.6).
narrative_ontology:measurement_basis(sgc_identity_read_su_t12, observed).
narrative_ontology:measurement(sgc_identity_read_su_t14, sex_gender_category__identity_reading, suppression_requirement, 14, 0.57).
narrative_ontology:measurement_basis(sgc_identity_read_su_t14, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'who counts as a woman' covers three structurally distinct determination rules with disjoint victim sets and different enforcement economics. Each reading is a separate story with its own epsilon, beneficiaries, victims, and classification; the family is linked through affects_constraints. The upstream/downstream structure runs through shared doctrine: Yogyakarta-principles lineage and Goodwin-line jurisprudence feed all three readings, but each converts that inheritance into a different membership rule, so contamination propagates between siblings through the shared doctrinal base rather than through one story carrying a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
