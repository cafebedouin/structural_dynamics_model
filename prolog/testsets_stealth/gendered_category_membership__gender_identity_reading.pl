% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__gender_identity_reading, []).

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
 *   constraint_id: gendered_category_membership__gender_identity_reading
 *   human_readable: Self-Declaration Norm for Gendered Category Membership
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   gendered_category_membership: the self-declaration reading, under which
 *   membership in the gendered category 'woman' (and correspondingly 'man')
 *   is constituted by sincere first-person declaration. The arrangement
 *   described is the declaration-based regime as it operates in jurisdictions
 *   and institutions that have adopted it: intake by declaration, enforcement
 *   through anti-discrimination frameworks and social sanction, and the
 *   recoding of resistance to the norm as wrongful exclusion.
 *   Constraint-family note (required by the decomposition pattern): the
 *   colloquial label 'what makes someone a woman' decomposes into three
 *   structurally distinct constraints — this file, the biological-marker
 *   reading, and the social-performance reading — linked via
 *   network.affects_constraints; each has its own epsilon, victim set, and
 *   enforcement direction. This file's epsilon is authored for the
 *   declaration-based arrangement itself, by this reading's own lights:
 *   moderate, concentrated in the costs of enforcing consensus and in
 *   consent-free boundary revision borne by specific seats. KEY AGENTS (by
 *   structural relationship): see key_agents; the beneficiary seat gains
 *   access by declaration, the payer seats bear converted guarantees and
 *   sanction costs, and the agenda-setter seats codify and administer.
 *
 * KEY AGENTS:
 *   - - transgender_people_seeking_recognition: Primary beneficiary (organized/trapped) — gains category access by declaration; bore gatekeeping costs under prior regimes; cannot exit gendered sorting
 *   - - women_in_sex_specific_settings: Primary payer (moderate/constrained) — bear conversion of same-sex service guarantees into declaration-based ones they did not consent to
 *   - - institutional_administrators: Agenda-setter and secondary payer (institutional/constrained) — implement the rule daily, inherit contested-case liability in both directions
 *   - - legislative_and_judicial_bodies: Codifying agenda-setter (institutional/mobile) — entrench or reverse the criterion at electoral and appellate intervals
 *   - - sanctioned_dissenters: Payer (organized/identity_locked) — bear professional and reputational sanction for publicly declining the criterion; bound through their opposition
 *   - - category_theorists_bioethicists: Analytical observer (analytical/analytical) — analyze the criterion's ontology without operational stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, 0.52).
domain_priors:suppression_score(gendered_category_membership__gender_identity_reading, 0.65).
domain_priors:theater_ratio(gendered_category_membership__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(gendered_category_membership__gender_identity_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__gender_identity_reading, "Self-Declaration Norm for Gendered Category Membership").
narrative_ontology:topic_domain(gendered_category_membership__gender_identity_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__gender_identity_reading, '10a19675-9bd1-4a04-98a4-ebbce8905661').
narrative_ontology:cs_kernel_codification('10a19675-9bd1-4a04-98a4-ebbce8905661', distributed).
narrative_ontology:cs_authority_grounding('10a19675-9bd1-4a04-98a4-ebbce8905661', diffuse_epistemic).
narrative_ontology:cs_reading_relation('10a19675-9bd1-4a04-98a4-ebbce8905661', gendered_category_membership__biological_sex_reading, forecloses).
narrative_ontology:cs_reading_relation('10a19675-9bd1-4a04-98a4-ebbce8905661', gendered_category_membership__social_role_reading, coexists_with).
narrative_ontology:cs_axiom('10a19675-9bd1-4a04-98a4-ebbce8905661', foundational, self_declaration_constitutes_membership).
narrative_ontology:cs_axiom_status(self_declaration_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('10a19675-9bd1-4a04-98a4-ebbce8905661', self_declaration_constitutes_membership, deontological).
narrative_ontology:cs_axiom('10a19675-9bd1-4a04-98a4-ebbce8905661', secondary, third_party_gatekeeping_is_illegitimate).
narrative_ontology:cs_axiom_status(third_party_gatekeeping_is_illegitimate, holdable).
narrative_ontology:cs_axiom_grounding('10a19675-9bd1-4a04-98a4-ebbce8905661', third_party_gatekeeping_is_illegitimate, instrumental).
narrative_ontology:cs_reference_frame('10a19675-9bd1-4a04-98a4-ebbce8905661', self_declaration_default_rule).
narrative_ontology:cs_drift_state('10a19675-9bd1-4a04-98a4-ebbce8905661', contemporary_partial_codification, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('10a19675-9bd1-4a04-98a4-ebbce8905661', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__gender_identity_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__gender_identity_reading, transgender_people_seeking_recognition).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, women_in_sex_specific_settings).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, sanctioned_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(gendered_category_membership__gender_identity_reading, institutional_administrators).
narrative_ontology:constraint_vindicates(gendered_category_membership__gender_identity_reading, first_person_authority_over_identity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain legal and social recognition, documents, and access to gendered facilities and services by sincere declaration alone where the norm holds. Under prior regimes they bore medicalized gatekeeping: diagnosis requirements, approval panels, waiting periods, and in some jurisdictions sterilization prerequisites. They cannot opt out of being categorized at all — everyone is sorted by gender — so their stake is entirely in which criterion governs, and they are bound to the outcome wherever they live.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, transgender_people_seeking_recognition, beneficiary,
    organized, biographical, trapped, global).

% Users and staff of refuges, shelters, prisons, hospital wards, and changing rooms — services whose same-sex composition was guaranteed implicitly by the prior criterion. Under declaration-based membership that guarantee converts to a declaration-based one they did not individually agree to. Raising the objection inside these institutions risks sanction under the very norms that govern them, and forgoing the service (the nominal exit) is often not realistic for residents, patients, or inmates.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, women_in_sex_specific_settings, payer,
    moderate, biographical, constrained, global).

% Prison services, shelter networks, sports federations, schools, and employers implement declaration-based rules in intake, housing, eligibility, and records. They gain relief from operating verification panels but inherit contested-case adjudication and liability exposure running in both directions — excluding a declarant exposes them to discrimination findings, while other users' objections expose them to negligence or safety claims. Most did not author the norm; they are its daily face.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, institutional_administrators, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__gender_identity_reading, institutional_administrators, payer).

% Codify, refuse, or revise the criterion through statute, tribunal doctrine, and regulatory guidance, at electoral and appellate intervals. Several jurisdictions have moved in both directions across the interval — adopting declaration regimes in some cases, declining or narrowing them in others — and each entrenchment carries political cost that later majorities can attempt to unwind.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, legislative_and_judicial_bodies, agenda_setter,
    institutional, generational, mobile, national).

% Academics, clinicians, writers, and ordinary participants who publicly decline the declaration criterion or assert competing criteria. They bear professional, reputational, and platform-level sanctions: lost employment or referrals, tribunal findings, dereferencing, social ostracism. Their public position is constituted by the objection itself, so exiting the dispute would mean abandoning the position — the constraint binds them precisely through their opposition.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, sanctioned_dissenters, payer,
    organized, biographical, identity_locked, global).

% Philosophers, sociologists, and bioethicists analyzing what kind of category 'woman' is and what work each candidate criterion performs. They publish on all three groundings, track jurisdictional divergence, and hold no operational stake in which criterion prevails beyond scholarly position-taking.
narrative_ontology:constraint_stakeholder(gendered_category_membership__gender_identity_reading, category_theorists_bioethicists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__gender_identity_reading, diffuse).
narrative_ontology:fixing_cost_class(gendered_category_membership__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a universally applicable, zero-verification default rule for gendered social sorting: take declared identity at face value. This replaces criteria that are either unadministerable (no external test accesses subjective identity) or administered only through costly, humiliating gatekeeping machinery, and it gives institutions a single simple intake rule.
% TRANSFER_FUNCTION: Moves category-definitional authority from external criteria and their administering bodies to individual declaration. Moves the historical costs of gatekeeping off those seeking recognition and onto institutions (adjudication of contested cases) and onto members of the previously bounded category (loss of exclusive boundary control in shared settings). Moves reputational and professional risk onto those who publicly decline the norm.
% ABSENT_VOICES: People who bear the costs but lacked standing where the norm was codified: residents of refuges and prisons consulted after admission rules changed, clinicians with ward-level objections filtered out before policy review, and populations in jurisdictions where the norm was adopted by elite consensus without broad deliberation. Detransitioned people occupy an awkward seat — their testimony cuts across the beneficiary category they are assigned to.
% DISAPPEARANCE_RATIONALE: If the declaration norm vanished overnight, every adopting jurisdiction would revert to some verification criterion — medical, anatomical, or performance-based — reopening gatekeeping panels, redocumentation, and access disputes across prisons, shelters, sports, and records systems. Thousands of administrative arrangements, employment codes, and tribunal precedents would need rewiring, and the population that gained access by declaration would lose it.
% FOUNDING_PROBLEM: Gendered category membership, when grounded in externally verifiable criteria, imposed severe and sometimes impossible burdens on trans people: psychiatric gatekeeping panels, multi-year waiting requirements, mandatory medical interventions, and in earlier statutory schemes sterilization prerequisites — all to obtain recognition of an identity no external examiner could access directly.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: European Court of Human Rights jurisprudence (from Goodwin v. UK onward) found refusal of recognition placed applicants in an intolerable position; published clinical literature across decades documents the harms of gatekeeping requirements, authored largely by clinicians who are not category beneficiaries; comparative-law surveys attest that gatekeeping statutes persist across much of the world, confirming the founding problem is not yet dead.
narrative_ontology:disappearance_verdict(gendered_category_membership__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__gender_identity_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__gender_identity_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.52 at interval end) per this reading's own structural delta: the arrangement's residual extraction consists of gatekeeping-adjacent costs — consent-free revision of service guarantees borne by women_in_sex_specific_settings, adjudication liability borne by administrators, and sanction costs borne by dissenters — offset by a large genuine liberation from prior gatekeeping. Suppression (0.65) is the arrangement's most distinctive feature: the norm converts a policy dispute into a character verdict in advance, so the alternative (publicly asserting a competing criterion) carries escalating professional and reputational price; suppression is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled downstream. Theater ratio is low-moderate (0.28): the sorting function is real and mostly works, but performative compliance (branding, ritual signaling) grows as the norm diffuses. Accessibility_collapse (0.55) is moderate: within adopting jurisdictions the competing criterion becomes legally unavailable, but globally alternatives persist and several polities have moved the other way. Resistance (0.60) is substantial and organized — payer seats have formed partial coalitions (women's organizations allied with dissenting academics and clinicians), which is why resistance is high despite the sanction environment. Claim/metric independence: claimed_type is tangled_rope because the structure genuinely possesses both a coordination function (a workable universal sorting rule; abolition of humiliating gatekeeping) and asymmetric extraction (uncompensated cost imposition plus dissent suppression) held together by active enforcement — the metrics were authored independently from that structural judgment, not tuned to it. Measurement series run on one shared quinquennial grid (2004-2024) so every tracked metric is authored at every examined time point; the trajectories are directional rather than cyclical, with contest waves (e.g., 2018-2020 UK deliberation, subsequent reversals elsewhere) smoothed by the grid spacing. Receipt surface: gain_flow is authored 'diffuse' — the extracted value (boundary control, dissent-silenced margin) dissipates rather than accruing to any seat: definitional authority dissolves into per-person self-authority rather than transferring to a capturer, and no stakeholder situation describes a seat pocketing the extraction; the access gains trans people receive are the coordination output, not the extracted residue. fixing_cost is 'prohibitive': for any seat able to fix (legislatures), unwinding the norm costs more than fixing yields — entrenched administrative practice, human-rights litigation exposure, and guaranteed counter-mobilization — which is why the arrangement persists through intense contest. This combination is NOT the piton cell in substance: theater is low, the function is live, and the stickiness is political economy, not inertia.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the beneficiary seat the arrangement is close to pure coordination: it abolished panels, diagnoses, and sterilization prerequisites and replaced them with a sentence. From the payer seats the same structure operates as a boundary redrawn without consent and defended by sanction — women_in_sex_specific_settings experience the loss of a guarantee they never voted away, and sanctioned_dissenters experience a discourse where their objection is pre-classified as wrongdoing. From the administrator seat it is a simplification that imports liability: cheaper intake, costlier edge cases. From the codifying seat it is a distributive choice between constituencies. The engine computes these per-seat classifications from the structural data; the divergence between the beneficiary seat's near-rope experience and the payer seats' extraction experience is the measurable content of this story.
 *
 * DIRECTIONALITY LOGIC:
 *   transgender_people_seeking_recognition sits near the beneficiary pole (d low): the constraint subsidizes them with access, and their trapped exit position deepens their stake without making them targets. women_in_sex_specific_settings sit near the target pole (d high): they bear the transferred costs with constrained exit, so effective extraction is amplified for them. sanctioned_dissenters also sit near the target pole despite organized power — the sanction operates on speech and reputation, where organization purchases less protection than it would in resource conflicts, and their identity_locked exit keeps them at the exposed end. institutional_administrators derive a mid-low d: they are not listed as victims because their net position is administrative gain (verification burden removed) partially offset by liability they absorb as payers; the secondary_role marks the mix. legislative_and_judicial_bodies are near-symmetric agenda-setters — they set the rule and bear its political costs. National codification scopes mean scope-amplification of extraction is modest but nonzero; the global diffusion of the norm raises verification difficulty at the margins.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — punitive and medically invasive gatekeeping of category recognition — is still live in most of the world, so this constraint has not outlived its mandate; mandatrophy_resolved is false and no sunset clause is declared. The tangled_rope classification prevents mislabeling in both directions: a pure-rope reading would erase the real costs borne by non-consenting payer seats and the sanction machinery aimed at dissent; a pure-snare reading would erase the genuine liberation the arrangement performs for its beneficiaries and mistake a live coordination function for cover. The classification holds both truths: coordination that extracts, enforced actively, with the extraction currently growing faster than the coordination benefit (see the suppression_requirement series).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is one reading (gender_identity_reading) of the kernel gendered_category_membership; what would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Comparative classification across the three family files: biological_sex_reading and social_role_reading each carry their own epsilon, victim set, and enforcement direction; the disagreement is located in the criterion of membership itself (declaration vs. immutable biological markers vs. sustained recognized performance).',
    'Under the biological reading the victim set relocates — would-be entrants denied access become the harmed class and enforcement runs toward excluding declarants; under the social-role reading membership becomes conditional on sustained recognized performance, reintroducing a performance gate. Cross-reading comparison, not within-story hedging, is the resolution path.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Committer structure: this file is the self-declaration reading of a three-reading kernel; sibling readings instantiate different constraints with different victim sets.').

omega_variable(
    cost_magnitude_in_sex_specific_settings,
    'What is the actual incidence and severity of costs borne by women in refuges, prisons, wards, and changing rooms under declaration-based admission, relative to the pre-change baseline?',
    'Service-level incident and outcome data collected without advocacy filtering — longitudinal comparisons of safety incidents, service uptake, and staff attrition before and after admission-rule changes across comparable institutions.',
    'If costs are negligible, the victim declaration for women_in_sex_specific_settings thins and the arrangement computes closer to rope; if material, the payer seats compute harder and the extraction asymmetry widens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cost_magnitude_in_sex_specific_settings, empirical, 'Whether the declared victim seat bears material costs or the costs are dominated by anticipatory objection.').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is the measured suppression of dissent structural (enforceable employment tribunals, HR codes, platform rules) or internalized (self-silencing that persists absent any filed complaint)?',
    'Post-sanction-regime speech trajectories and prevalence surveys of unstated objection: if self-censorship persists where enforcement capacity is weak or withdrawn, a large share is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure and would persist even if enforcement relaxed — raising the arrangement''s true suppression and complicating any liberalization remedy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Structural vs. internalized split of the dissent-chilling mechanism.').

omega_variable(
    enforcement_dependence_of_compliance,
    'Would declaration-based recognition persist if the sanction machinery relaxed — is everyday compliance voluntary coordination or enforced?',
    'Natural experiments in jurisdictions and institutions that softened enforcement (lapsed codes, deprioritized complaints): track whether declaration-based practice holds or reverts.',
    'Persistent voluntary compliance supports the rope component dominating; rapid reversion indicates the enforced-extraction component is load-bearing and the arrangement is more snare-flavored than its coordination surface suggests.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(enforcement_dependence_of_compliance, empirical, 'Dependence of the norm''s operation on active sanction versus voluntary uptake.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__gender_identity_reading, 2004, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gender_identity_reading_tr_t2004, gendered_category_membership__gender_identity_reading, theater_ratio, 2004, 0.1).
narrative_ontology:measurement_basis(gender_identity_reading_tr_t2004, observed).
narrative_ontology:measurement(gender_identity_reading_tr_t2009, gendered_category_membership__gender_identity_reading, theater_ratio, 2009, 0.12).
narrative_ontology:measurement_basis(gender_identity_reading_tr_t2009, observed).
narrative_ontology:measurement(gender_identity_reading_tr_t2014, gendered_category_membership__gender_identity_reading, theater_ratio, 2014, 0.16).
narrative_ontology:measurement_basis(gender_identity_reading_tr_t2014, observed).
narrative_ontology:measurement(gender_identity_reading_tr_t2019, gendered_category_membership__gender_identity_reading, theater_ratio, 2019, 0.22).
narrative_ontology:measurement_basis(gender_identity_reading_tr_t2019, observed).
narrative_ontology:measurement(gender_identity_reading_tr_t2024, gendered_category_membership__gender_identity_reading, theater_ratio, 2024, 0.28).
narrative_ontology:measurement_basis(gender_identity_reading_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(gender_identity_reading_be_t2004, gendered_category_membership__gender_identity_reading, base_extractiveness, 2004, 0.34).
narrative_ontology:measurement_basis(gender_identity_reading_be_t2004, observed).
narrative_ontology:measurement(gender_identity_reading_be_t2009, gendered_category_membership__gender_identity_reading, base_extractiveness, 2009, 0.37).
narrative_ontology:measurement_basis(gender_identity_reading_be_t2009, observed).
narrative_ontology:measurement(gender_identity_reading_be_t2014, gendered_category_membership__gender_identity_reading, base_extractiveness, 2014, 0.42).
narrative_ontology:measurement_basis(gender_identity_reading_be_t2014, observed).
narrative_ontology:measurement(gender_identity_reading_be_t2019, gendered_category_membership__gender_identity_reading, base_extractiveness, 2019, 0.47).
narrative_ontology:measurement_basis(gender_identity_reading_be_t2019, observed).
narrative_ontology:measurement(gender_identity_reading_be_t2024, gendered_category_membership__gender_identity_reading, base_extractiveness, 2024, 0.52).
narrative_ontology:measurement_basis(gender_identity_reading_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(gender_identity_reading_su_t2004, gendered_category_membership__gender_identity_reading, suppression_requirement, 2004, 0.25).
narrative_ontology:measurement_basis(gender_identity_reading_su_t2004, observed).
narrative_ontology:measurement(gender_identity_reading_su_t2009, gendered_category_membership__gender_identity_reading, suppression_requirement, 2009, 0.32).
narrative_ontology:measurement_basis(gender_identity_reading_su_t2009, observed).
narrative_ontology:measurement(gender_identity_reading_su_t2014, gendered_category_membership__gender_identity_reading, suppression_requirement, 2014, 0.42).
narrative_ontology:measurement_basis(gender_identity_reading_su_t2014, observed).
narrative_ontology:measurement(gender_identity_reading_su_t2019, gendered_category_membership__gender_identity_reading, suppression_requirement, 2019, 0.56).
narrative_ontology:measurement_basis(gender_identity_reading_su_t2019, observed).
narrative_ontology:measurement(gender_identity_reading_su_t2024, gendered_category_membership__gender_identity_reading, suppression_requirement, 2024, 0.65).
narrative_ontology:measurement_basis(gender_identity_reading_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__gender_identity_reading, gendered_category_membership__social_role_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what makes someone a woman' decomposes into three structurally distinct constraints per the epsilon-invariance principle — measuring membership by declaration, by immutable biological markers, or by sustained recognized performance yields different epsilon values, different victim sets, and opposite enforcement directions, so they are modeled as three linked stories rather than one story with a measurement parameter. This file is the self-declaration reading; it links to both siblings. The upstream/downstream citation pattern runs in both directions here: each reading cites the others' failures as evidence for itself.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
