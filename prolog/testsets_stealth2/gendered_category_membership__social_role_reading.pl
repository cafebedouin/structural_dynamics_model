% ============================================================================
% CONSTRAINT STORY: gendered_category_membership__social_role_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gendered_category_membership__social_role_reading, []).

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
 *   constraint_id: gendered_category_membership__social_role_reading
 *   human_readable: Recognition-Gated Gendered Category Membership (Social-Role Reading)
 *   domain: social ontology/political philosophy/bioethics
 *
 * SUMMARY:
 *   Under the social-role reading, membership in a gendered category is
 *   neither fixed at birth nor settled by self-declaration: it is constituted
 *   through sustained social performance — dress, grooming, voice, demeanor,
 *   occupational and relational conduct — and stabilized by ongoing
 *   recognition from others. Gatekeeping is radically distributed: no
 *   committee administers membership; millions of ordinary interactions grant
 *   or withhold it. Trans women are included conditionally, to the extent
 *   their performance passes and recognition accumulates, and membership
 *   remains revocable on detection or dispute. The arrangement solves a real
 *   coordination problem (mutual legibility of category members) while
 *   imposing asymmetric performance costs and exclusion risk on those whose
 *   membership is not naturalized. KEY AGENTS (by structural relationship): -
 *   trans_women_seeking_recognition: Primary target
 *   (moderate/identity_locked) — bears performance costs and revocable
 *   membership - naturally_recognized_cis_women: Primary beneficiary
 *   (organized/constrained) — collects default recognition, pays only
 *   ordinary presentation norms - naturally_recognized_cis_men: Secondary
 *   beneficiary (organized/constrained) — mirror seat on the less-contested
 *   boundary - everyday_gatekeepers: Distributed administrator
 *   (organized/mobile) — grants/withholds recognition in micro-interactions
 *   while remaining subject to the norms - gender_nonconforming_cis_women:
 *   Dual-positioned payer-beneficiary (moderate/constrained) -
 *   nonbinary_people_outside_binary: Excluded voice (moderate/constrained) —
 *   outside the binary the reading adjudicates - gender_studies_scholars:
 *   Analytical observer (analytical/analytical) — sees the full structure
 *   Constraint-family note: this story is one reading of the
 *   gendered_category_membership kernel. Its ε (0.48) is indexed to the
 *   performance-recognition arrangement specifically. The
 *   biological_sex_reading sibling concentrates exclusion wholly on trans
 *   people regardless of performance (higher ε, one-sided victim set); the
 *   gender_identity_reading sibling reduces the victim set to misdeclaration
 *   disputes (lower ε). The readings are separate constraints with separate
 *   files, linked via network.affects_constraints.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, 0.48).
domain_priors:suppression_score(gendered_category_membership__social_role_reading, 0.5).
domain_priors:theater_ratio(gendered_category_membership__social_role_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(gendered_category_membership__social_role_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gendered_category_membership__social_role_reading, tangled_rope).
narrative_ontology:human_readable(gendered_category_membership__social_role_reading, "Recognition-Gated Gendered Category Membership (Social-Role Reading)").
narrative_ontology:topic_domain(gendered_category_membership__social_role_reading, "social ontology/political philosophy/bioethics").

domain_priors:requires_active_enforcement(gendered_category_membership__social_role_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(gendered_category_membership__social_role_reading, '21d69654-d650-41f0-a583-816ab70d760f').
narrative_ontology:cs_kernel_codification('21d69654-d650-41f0-a583-816ab70d760f', distributed).
narrative_ontology:cs_authority_grounding('21d69654-d650-41f0-a583-816ab70d760f', practice).
narrative_ontology:cs_interpretation_layer_present('21d69654-d650-41f0-a583-816ab70d760f').
narrative_ontology:cs_reading_relation('21d69654-d650-41f0-a583-816ab70d760f', gendered_category_membership__biological_sex_reading, coexists_with).
narrative_ontology:cs_reading_relation('21d69654-d650-41f0-a583-816ab70d760f', gendered_category_membership__gender_identity_reading, influences).
narrative_ontology:cs_axiom('21d69654-d650-41f0-a583-816ab70d760f', foundational, membership_constituted_by_recognized_performance).
narrative_ontology:cs_axiom_status(membership_constituted_by_recognized_performance, holdable).
narrative_ontology:cs_axiom_grounding('21d69654-d650-41f0-a583-816ab70d760f', membership_constituted_by_recognized_performance, conventional).
narrative_ontology:cs_axiom('21d69654-d650-41f0-a583-816ab70d760f', secondary, conditional_inclusion_via_passing).
narrative_ontology:cs_axiom_status(conditional_inclusion_via_passing, holdable).
narrative_ontology:cs_axiom_grounding('21d69654-d650-41f0-a583-816ab70d760f', conditional_inclusion_via_passing, instrumental).
narrative_ontology:cs_reference_frame('21d69654-d650-41f0-a583-816ab70d760f', sustained_performance_recognition_constitutes_membership).
narrative_ontology:cs_drift_state('21d69654-d650-41f0-a583-816ab70d760f', contemporary_trans_visibility_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('21d69654-d650-41f0-a583-816ab70d760f', '').
narrative_ontology:cs_kernel_id(gendered_category_membership__social_role_reading, gendered_category_membership).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, naturally_recognized_cis_women).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, naturally_recognized_cis_men).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, trans_women_seeking_recognition).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women).
narrative_ontology:constraint_victim(gendered_category_membership__social_role_reading, naturally_recognized_cis_women).
narrative_ontology:constraint_vindicates(gendered_category_membership__social_role_reading, collective_recognition_constitutes_social_kinds).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Membership is confirmed by default in virtually every interaction without deliberation or doubt. They pay the category's ongoing presentation norms — grooming, dress, demeanor, emotional labor expectations — but never face the membership question itself; the recognition machinery is calibrated to their unmarked presentation. Exiting the category's norms carries social cost, while their membership is never at risk.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, naturally_recognized_cis_women, beneficiary,
    organized, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, naturally_recognized_cis_women, payer).

% Mirror-image seat on the less-contested boundary. They collect default recognition of manhood and pay masculinity performance norms (toughness displays, emotional restriction) enforced largely by each other. Because the live membership contests center on womanhood, their benefit is mostly unexamined background coordination.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, naturally_recognized_cis_men, beneficiary,
    organized, biographical, constrained, global).

% Must sustain a legible performance across every interaction to accumulate and keep recognition; membership is never fully secured, and a single detected inconsistency or public dispute can revoke it. Costs include continuous presentation labor, the economic burden of passing (clothing, grooming, medical intervention where sought), and chronic exclusion risk from gendered spaces and roles. Abandoning the pursuit means living under permanent misrecognition, which the identity project itself cannot absorb — exit is not a live option from where they stand.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, trans_women_seeking_recognition, payer,
    moderate, biographical, identity_locked, global).

% Hold birth-default membership yet attract gatekeeping whenever presentation departs from the recognized script: butch and masculine-presenting women report routine challenges to facility access, address, and belonging. They benefit from the category's default protections while paying scrutiny costs the naturally recognized avoid — a dual position inside the same structure.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women, payer,
    moderate, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(gendered_category_membership__social_role_reading, gender_nonconforming_cis_women, beneficiary).

% Strangers, coworkers, family members, and service staff who grant or withhold recognition in ordinary interactions — a glance, a pronoun, a door held or blocked. No central authority directs them, yet collectively their responses constitute the enforcement apparatus. Each individual can decline to police any given interaction at no personal cost, which is why enforcement is distributed rather than commanded; each is also a gendered subject subject to the same norms they administer.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, everyday_gatekeepers, agenda_setter,
    organized, biographical, mobile, global).

% The reading presupposes exactly two categories and adjudicates only who qualifies for each. People outside the binary have no membership question they can win: the recognition machinery offers them misclassification in one direction or the other. They would object that the kernel's binary structure, not merely its gatekeeping, is the injury — but they have no seat in the conversation this reading frames.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, nonbinary_people_outside_binary, excluded,
    moderate, biographical, constrained, global).

% Analyze how category membership is constituted, document the costs of recognition-gating, and supply the conceptual vocabulary (performativity, passing, cisnormativity) that both defenders and critics of the reading use. They collect no rents from the arrangement and bear no gatekeeping; their seat sees the full structure.
narrative_ontology:constraint_stakeholder(gendered_category_membership__social_role_reading, gender_studies_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(gendered_category_membership__social_role_reading, naturally_recognized_cis_women).
narrative_ontology:fixing_cost_class(gendered_category_membership__social_role_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Produces mutually legible category members so that strangers can coordinate interaction — pronouns, spatial conventions, forms of address, role expectations — without negotiating each encounter. Sustained performance makes membership detectable; mutual recognition stabilizes it.
% TRANSFER_FUNCTION: Moves recognition, belonging, and access to gendered spaces and roles to those who sustain a detectable performance; moves continuous presentation labor, self-monitoring, and exclusion risk from members into maintenance of the category system — concentrating both on those whose membership is not naturalized.
% ABSENT_VOICES: Nonbinary and genderqueer people have no seat: the reading presupposes two categories and adjudicates only who qualifies for each. Children socialized into performance before any capacity to consent are also absent. Those who fail recognition speak only insofar as they pass.
% DISAPPEARANCE_RATIONALE: Everyday interaction runs on the category cues this regime produces. Overnight removal would force renegotiation of address forms, spatial conventions, and role expectations across millions of simultaneous micro-interactions, and institutions keyed to recognized membership — facilities, sports, record-keeping, kinship terminology — would lose their sorting criterion until replaced.
% FOUNDING_PROBLEM: Making sexually differentiated bodies socially legible so that a division of labor, space, and obligation could be coordinated without case-by-case negotiation — marking who is eligible for which roles, protections, and burdens.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological and historical scholarship on sex/gender systems attests the coordination origin from outside the benefiting parties (cross-cultural records of gender organizing labor, ritual, and space allocation). Queer-theory and trans-studies scholarship attests that most allocation functions have since detached from performance-based membership in contemporary institutions. Defenders inside the beneficiary set attest the legibility problem is still live; no extra-party source settles the dispute — hence contested.
narrative_ontology:disappearance_verdict(gendered_category_membership__social_role_reading, world_rearranges).
narrative_ontology:founding_problem_status(gendered_category_membership__social_role_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(gendered_category_membership__social_role_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(gendered_category_membership__social_role_reading, 'none', 1).
narrative_ontology:epsilon_provenance(gendered_category_membership__social_role_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gendered_category_membership__social_role_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(gendered_category_membership__social_role_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(gendered_category_membership__social_role_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.48): the performance burden — presentation labor, self-monitoring, the economic cost of passing, chronic exclusion risk — is real and concentrated on non-naturalized members, but it is paid partly as the entry price of a coordination scheme that also returns belonging and legibility. Suppression (0.50) is structural and distributed: micro-sanctions, misrecognition, and informal exclusion rather than concentrated command; it is authored as a raw structural property and is NOT scaled by power or scope — the engine scales only extractiveness. Theater ratio (0.30): as law and market absorbed most of the old role-allocation work, a growing share of enforcement activity defends the boundary qua boundary rather than performing residual coordination, but most interactive enforcement still does legibility work. Accessibility collapse (0.40): alternatives remain partly available — the sibling readings are live competing accounts and some people live outside the binary — but exit carries heavy cost. Resistance (0.60): trans advocacy, deliberate non-performance, and scholarly critique actively contest the gatekeeping, with coalition potential across trans, gender-nonconforming, and nonbinary constituencies. claimed_type is authored independently as tangled_rope: both a genuine coordination function and asymmetric extraction are structurally present. The metrics were authored descriptively, not tuned to the claim or to any predicted engine output.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the trans women's seat the arrangement operates as enforced extraction with revocable membership — high directionality, amplified by identity-locked exit. From the naturally recognized cis women's seat the same structure is experienced as benign background coordination: they have never faced the gate, so the extraction is invisible from where they stand. Everyday gatekeepers experience administering it as ordinary politeness or perceptual honesty. The engine computes these per-seat classifications from the structural data; the divergence between 'this is just how recognition works' and 'this is a toll I pay forever' is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low d: naturally_recognized_cis_women and naturally_recognized_cis_men collect default recognition while paying only ordinary presentation norms. Victim declarations map to high d: trans_women_seeking_recognition bear the transfer (performance labor flowing into category maintenance), with identity-locked exit pushing them toward the full-target end; gender_nonconforming_cis_women sit intermediate — default membership but real scrutiny. Everyday_gatekeepers are listed as neither beneficiaries nor victims: they administer the constraint while remaining subject to it, placing them near symmetric. Nonbinary people stand outside the binary the constraint adjudicates — exposed to misclassification rather than to membership terms. Global spatial scope modestly amplifies effective extraction for targets by making recognition standards unverifiable across contexts. Receipt: the gains demonstrably land on naturally_recognized_cis_women — the category's norms are calibrated to their unmarked presentation, so the extracted performance labor maintains a category whose benefits accrue disproportionately there; fixing_cost is prohibitive because no single actor can flip distributed enforcement, and replacing the criterion means reorganizing millions of micro-interactions and institutional defaults relative to any one seat's benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — making sexually differentiated bodies legible so a division of labor, space, and obligation could run without negotiation — has partially receded: law, market, and bureaucracy now allocate most roles by rule rather than by recognized performance. What remains load-bearing is interpersonal legibility. The tangled_rope classification prevents both mislabels: reading the arrangement as snare would erase the genuine coordination function (strangers do coordinate smoothly through it); reading it as rope would erase the concentrated, revocable-cost structure borne by those who must earn what others inherit. Mandatrophy is not resolved: the mandate is contested, partially atrophied, and still load-bearing — hence founding_problem_status 'contested' rather than 'dead', and theater_ratio well below piton range. Boltzmann note: identity_coordination is declared because the constraint's primary function is genuine boundary-and-membership coordination; the FNL gaming check is flagged — 'this is just how our culture works' is exactly the cover-story shape, and the coupling profile (extraction concentrated on lower-power agents at global scope) warrants review rather than default tolerance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positionality,
    'This story instantiates one reading of the gendered_category_membership kernel; would the biological_sex_reading or gender_identity_reading change the structural classification of the same interaction sites?',
    'Compile the sibling stories and compare computed types and epsilon over the same sites (facilities, sports, documents, everyday address).',
    'The biological reading concentrates victims wholly in trans people (higher epsilon, snare-flavored); the identity reading shrinks victims to misdeclaration disputes (lower epsilon, rope-flavored). The tangled_rope verdict here holds only for the performance-recognition criterion — it is not a verdict on the kernel.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positionality, conceptual, 'Committer-frame positionality: one of three rival readings of a shared kernel.').

omega_variable(
    victim_structure_symmetry,
    'Do cis women bear genuine exclusion risk under this reading, or is their risk marginal enough that the victim structure is effectively one-sided?',
    'Comparative audit of gatekeeping incidents by birth-assigned status: frequency and severity of membership challenges against gender-nonconforming cis women versus trans women.',
    'If cis-woman risk is marginal, beneficiaries dominate and the extraction asymmetry widens (snare-leaning); if substantial, the constraint approaches symmetric burden (rope-leaning). The ambiguous victim structure is the hinge of the classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_structure_symmetry, empirical, 'Whether the declared victim set is genuinely two-sided or nominally so.').

omega_variable(
    internalized_vs_structural_enforcement,
    'Is the performance burden maintained by external micro-sanction or by internalized norm compliance that persists without watchers?',
    'Compare performance behavior across anonymity gradients (pseudonymous online settings, diaspora communities, private settings); persistence of performance without observers indicates internalization.',
    'A large internalized share raises effective suppression above the structural measure and stabilizes the constraint against enforcement decay; a structural share makes it responsive to anti-discrimination norms and enforcement attrition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_vs_structural_enforcement, empirical, 'Structural versus internalized suppression mechanism in a distributed-enforcement constraint.').

omega_variable(
    passing_threshold_indeterminacy,
    'Where does the recognition threshold sit, and does its community-level variance undermine a single epsilon for the reading?',
    'Measure inclusion outcomes for identical presentations across community types (urban, rural, religious, generational cohorts).',
    'Wide variance means epsilon is population-weighted; the constraint could decompose into regional or cohort sub-constraints with different types rather than one averaged tangled_rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(passing_threshold_indeterminacy, conceptual, 'Threshold indeterminacy in a distributedly adjudicated criterion.').

omega_variable(
    coordination_extraction_separability,
    'Is the legibility function of the category system separable from the revocability and exclusion machinery that enforces it?',
    'Observe jurisdictions and communities that shift membership determination toward self-declaration while retaining category infrastructure: if legibility and coordination outcomes hold while revocable gatekeeping recedes, the functions are separable.',
    'If separable, the extraction rides on a real coordination function and remedies can target the gatekeeping alone; if inseparable, part of the measured epsilon is the price of legibility itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the constraint''s coordination and extraction components are structurally separable.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gendered_category_membership__social_role_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gend_tr_t0, gendered_category_membership__social_role_reading, theater_ratio, 0, 0.22).
narrative_ontology:measurement(gend_tr_t6, gendered_category_membership__social_role_reading, theater_ratio, 6, 0.24).
narrative_ontology:measurement(gend_tr_t12, gendered_category_membership__social_role_reading, theater_ratio, 12, 0.26).
narrative_ontology:measurement(gend_tr_t18, gendered_category_membership__social_role_reading, theater_ratio, 18, 0.28).
narrative_ontology:measurement(gend_tr_t24, gendered_category_membership__social_role_reading, theater_ratio, 24, 0.29).
narrative_ontology:measurement(gend_tr_t30, gendered_category_membership__social_role_reading, theater_ratio, 30, 0.3).

% Extraction over time
narrative_ontology:measurement(gend_be_t0, gendered_category_membership__social_role_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(gend_be_t6, gendered_category_membership__social_role_reading, base_extractiveness, 6, 0.42).
narrative_ontology:measurement(gend_be_t12, gendered_category_membership__social_role_reading, base_extractiveness, 12, 0.45).
narrative_ontology:measurement(gend_be_t18, gendered_category_membership__social_role_reading, base_extractiveness, 18, 0.47).
narrative_ontology:measurement(gend_be_t24, gendered_category_membership__social_role_reading, base_extractiveness, 24, 0.48).
narrative_ontology:measurement(gend_be_t30, gendered_category_membership__social_role_reading, base_extractiveness, 30, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(gend_su_t0, gendered_category_membership__social_role_reading, suppression_requirement, 0, 0.44).
narrative_ontology:measurement(gend_su_t6, gendered_category_membership__social_role_reading, suppression_requirement, 6, 0.46).
narrative_ontology:measurement(gend_su_t12, gendered_category_membership__social_role_reading, suppression_requirement, 12, 0.49).
narrative_ontology:measurement(gend_su_t18, gendered_category_membership__social_role_reading, suppression_requirement, 18, 0.51).
narrative_ontology:measurement(gend_su_t24, gendered_category_membership__social_role_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement(gend_su_t30, gendered_category_membership__social_role_reading, suppression_requirement, 30, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gendered_category_membership__social_role_reading, identity_coordination).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, biological_sex_reading).
narrative_ontology:affects_constraint(gendered_category_membership__social_role_reading, gender_identity_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'what makes someone a man or a woman?' decomposes into three structurally distinct constraints — biological-marker grounding, self-declaration grounding, and performance-recognition grounding — each with its own epsilon, victim set, and enforcement surface. This file instantiates the performance-recognition reading only: epsilon 0.48 reflecting performance costs and revocable membership, whereas the biological reading concentrates exclusion entirely on trans people (higher epsilon) and the identity reading reduces victims to misdeclaration disputes (lower epsilon). Sibling links route contamination-propagation analysis across the family; the upstream social-role regime structurally influences the identity reading's operating environment without foreclosing it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
