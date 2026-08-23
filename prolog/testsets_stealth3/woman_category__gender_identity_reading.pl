% ============================================================================
% CONSTRAINT STORY: woman_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__gender_identity_reading, []).

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
 *   constraint_id: woman_category__gender_identity_reading
 *   human_readable: Gender-Identity Reading of the 'Woman' Category Boundary
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   The arrangement this story models is the governance rule that category
 *   membership in 'woman' follows declared internal gender identity: document
 *   offices, service providers, sports bodies, and equality regulators admit
 *   anyone who identifies as a woman, and rules that reference natal sex to
 *   bound the category are classified as discrimination and dismantled.
 *   Adoption ran from niche medical-pathway reforms through statutory self-ID
 *   regimes to comprehensive institutional enforcement across employment,
 *   services, sport, and platform governance. The rule performs a real
 *   coordination function, namely uniform ungated recognition, while imposing
 *   asymmetric costs: competitive and safety losses concentrate on
 *   female-category athletes, placement and safeguarding changes fall on
 *   trapped service populations, sanction exposure falls on dissenters, and a
 *   permanent, individually carried burden of defending recognition falls on
 *   trans women themselves. Enforcement is active and intensifying;
 *   resistance is sustained and organized; the arrangement holds by statute,
 *   policy cascade, and litigation risk rather than by unanimity. The claimed
 *   type (tangled_rope) and the metrics below were authored independently:
 *   the claim states my structural judgment, the metrics state the
 *   arrangement's observed operation, and the engine computes per-seat
 *   classifications from the structural data. KEY AGENTS (by structural
 *   relationship): - transgender_women: primary beneficiary with secondary
 *   payer position (moderate/identity_locked) — receives recognition and
 *   access; bears the individually carried cost of defending each recognition
 *   event - lgbt_advocacy_organizations: secondary beneficiary and receipt
 *   seat (organized/mobile) — collects mandate, funding, and agenda authority
 *   scaled to the rule's institutional footprint - female_category_athletes:
 *   domain target in sport (moderate/constrained) — absorbs competitive,
 *   record, and safety losses where entry opens -
 *   sex_segregated_service_users: domain target in refuges, prisons, and
 *   wards (powerless/trapped) — experiences the changed composition of spaces
 *   they cannot leave - equality_regulators: agenda setter
 *   (institutional/arbitrage) — writes and enforces the rule in law; remit
 *   expands with adoption - sports_governing_bodies: agenda setter
 *   (institutional/constrained) — owns eligibility rules; hedges between the
 *   identity principle and operational criteria -
 *   sex_based_rights_dissenters: excluded (organized/barred from the venue) —
 *   sanctioned outside the rooms where the rule is drafted and reviewed -
 *   bioethics_legal_analysts: analytical observer — maps cost incidence and
 *   justification survival across domains
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__gender_identity_reading, 0.62).
domain_priors:suppression_score(woman_category__gender_identity_reading, 0.66).
domain_priors:theater_ratio(woman_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, suppression_requirement, 0.66).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(woman_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__gender_identity_reading, "Gender-Identity Reading of the 'Woman' Category Boundary").
narrative_ontology:topic_domain(woman_category__gender_identity_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__gender_identity_reading, '4c66bf78-51dd-4fab-9fff-f9bf7b236de3').
narrative_ontology:cs_kernel_codification('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', distributed).
narrative_ontology:cs_authority_grounding('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', distributed).
narrative_ontology:cs_reading_relation('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', woman_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', woman_category__intersex_accommodation_reading, forecloses).
narrative_ontology:cs_axiom('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', foundational, self_identification_determines_womanhood).
narrative_ontology:cs_axiom_status(self_identification_determines_womanhood, holdable).
narrative_ontology:cs_axiom_grounding('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', self_identification_determines_womanhood, deontological).
narrative_ontology:cs_axiom('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', secondary, sex_based_exclusion_constitutes_discrimination).
narrative_ontology:cs_axiom_status(sex_based_exclusion_constitutes_discrimination, holdable).
narrative_ontology:cs_axiom_grounding('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', sex_based_exclusion_constitutes_discrimination, deontological).
narrative_ontology:cs_reference_frame('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', identity_constitutes_membership).
narrative_ontology:cs_drift_state('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', contemporary_backlash_and_review_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('4c66bf78-51dd-4fab-9fff-f9bf7b236de3', '').
narrative_ontology:cs_kernel_id(woman_category__gender_identity_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_beneficiary(woman_category__gender_identity_reading, lgbt_advocacy_organizations).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, transgender_women).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, female_category_athletes).
narrative_ontology:constraint_victim(woman_category__gender_identity_reading, sex_segregated_service_users).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, self_identification_principle).
narrative_ontology:constraint_vindicates(woman_category__gender_identity_reading, gender_self_determination_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under the arrangement as its named constituency: the rule grants them membership in the category and access to women's documents, services, and spaces on the basis of self-declared identity. What flows to them is recognition and legal protection; what flows from them is the day-to-day work of defending that recognition, since each document correction, service admission, or eligibility claim is individually negotiated, litigated, or publicly contested, and hostile attention concentrates on them personally. Leaving the dispute is not available without renouncing the identity the category claim expresses.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, transgender_women, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(woman_category__gender_identity_reading, transgender_women, payer).

% Campaign for, draft, and monitor the arrangement's adoption across jurisdictions and institutions. The rule's expansion routes funding, consultancy engagements, statutory advisory roles, and agenda-setting authority to them; staffing and revenue scale with the rule's institutional footprint. Their exit is easy: expertise and networks transfer readily to adjacent rights work.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, lgbt_advocacy_organizations, beneficiary,
    organized, generational, mobile, national).

% Compete in women's categories whose entry criterion the arrangement sets by identity. Where entry is open, they absorb the competitive and safety consequences directly: podium places, records, roster spots, and in contact sports injury exposure. Publicly opposing the entry rule carries sponsorship, selection, and team-standing risk; their sporting careers bind them to the category structure they would have to critique from inside it.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, female_category_athletes, payer,
    moderate, biographical, constrained, global).

% Use refuges, prisons, hospital wards, and changing rooms provisioned on the assumption the arrangement has overwritten: that the women's category is biologically bounded. Women in crisis housing and incarcerated women cannot opt out of the facilities they are placed in; their exposure to the changed composition of these spaces is involuntary, and their complaints route through complaint systems operated by the same institutions implementing the rule.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_segregated_service_users, payer,
    powerless, immediate, trapped, national).

% Set and enforce the arrangement in law: rewrite equality guidance, adjudicate discrimination complaints, and require institutions to adopt identity-based entry. Each extension of the rule widens their enforcement remit and caseload; reversing it would shrink their mandate. They can shift enforcement intensity between domains at will.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, equality_regulators, agenda_setter,
    institutional, generational, arbitrage, national).

% Own the eligibility rules for women's competition. The arrangement pressures them to admit by identity; they respond with hedged implementations such as performance thresholds and case-by-case panels that preserve the identity principle nominally while reintroducing biological criteria operationally. International federation politics and litigation exposure limit how far they can move in either direction.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, constrained, global).

% Organize against the arrangement's core premise and seek to argue for biology-referenced boundaries in law and policy. Platform moderation, event exclusions, employment consequences, and party or association disciplinary rules keep them outside the consultation rooms where the rule is drafted and reviewed; legal challenge is currently the main channel through which they reach the conversation at all.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, sex_based_rights_dissenters, excluded,
    organized, biographical, trapped, national).

% Study the arrangement across its domains of application, including documents, sport, incarceration, and medicine, mapping where its costs land and which of its justifications survive scrutiny. They publish the domain-gradient evidence and the framing disputes; they collect nothing and pay nothing under the rule.
narrative_ontology:constraint_stakeholder(woman_category__gender_identity_reading, bioethics_legal_analysts, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__gender_identity_reading, lgbt_advocacy_organizations).
narrative_ontology:fixing_cost_class(woman_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one shared, medically ungated criterion for who counts as a woman across documents, services, sport, and data collection, replacing case-by-case biological adjudication with self-declaration; it lets institutions process recognition uniformly and lets trans people obtain documents and access without psychiatric or surgical gatekeeping.
% TRANSFER_FUNCTION: Moves category membership and the access attached to it to anyone who declares female gender identity; moves the authority to decide membership from medical and biological criteria to self-declaration; and moves the ongoing cost of holding the boundary, including litigation, complaint-handling, and social conflict, onto the individuals on both sides of it rather than onto the institutions that adopted the rule.
% ABSENT_VOICES: Sex-based-rights campaigners are outside the drafting and review rooms, kept there by platform moderation, party discipline, and litigation-channelled participation; detained women and refuge users have no seat in the consultations that reshape the spaces they inhabit; female athletes were consulted after eligibility frameworks were set, not before. Each would press the costs the arrangement currently books as prejudice.
% DISAPPEARANCE_RATIONALE: If the rule vanished overnight, document offices, prisons, refuges, and federations would revert to biological or adjudicated entry criteria within a planning cycle; trans people would lose the recognition pathway the rule provides and fall back to medical gatekeeping; the campaign, consultancy, and litigation economies built around the rule would collapse; and the boundary conflicts currently routed through individual access claims would re-emerge as institutional policy questions.
% FOUNDING_PROBLEM: Trans people had no route to legal and social recognition as women: documents were unchangeable without psychiatric diagnosis and often surgery, category protections were inaccessible, and medical gatekeeping committees decided who counted.
% FOUNDING_PROBLEM_CORROBORATION: The problem itself is corroborated from outside the beneficiary set: pre-reform legal records showing immutable documents, medical-literature documentation of gatekeeping harms, and human-rights bodies such as Council of Europe commissioner reports and UN independent-expert statements attesting recognition denial. Its current status is not corroborated as live by anyone outside the dispute: advocacy institutions attest it remains live; statutory review bodies and courts in several jurisdictions attest the recognition problem was substantially addressed by earlier medical-pathway reforms and that the remaining conflict is over boundary costs rather than recognition. No neutral attester resolves the dispute, and that absence is itself signal.
narrative_ontology:disappearance_verdict(woman_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__gender_identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__gender_identity_reading, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.62 aggregates a steep domain gradient: identity-document administration runs near 0.45, where the rule mostly removes gatekeeping, while elite-sport eligibility and sex-segregated placement run near 0.75, where access collisions are zero-sum with identifiable losers. Suppression 0.66 reflects the enforcement ratchet of statutory mandates, equality-guidance compulsion, and employment and platform sanction rather than participant preference; it is authored as a raw structural property and is deliberately left unscaled here, since the engine owns the directionality and scope arithmetic. Theater 0.28: adoption ceremonies, framework signatories, and diversity apparatus exceed implemented practice in places, but the enforcement core is functional. Accessibility_collapse 0.60: within adopting jurisdictions, biology-referenced alternatives become legally unavailable once the rule binds, while cross-jurisdiction movement, private-association rules, and litigation channels keep alternatives partly alive. Resistance 0.72: continuous organized contestation through campaigns, judicial challenges, and electoral reversals, among the highest of any arrangement in this corpus. The three measurement series share one seven-point grid (t=0 to t=24, anchored to 2000-2024) so no metric inherits another's end-state value. The trajectories are ratchet-shaped with a deceleration at t=16-20 marking the review-era pause before enforcement resumed; the oscillation is backlash-wave modulation superimposed on a ratchet, not a true cycle, and the intermittent-reinforcement question is left to the temporal analyzer rather than asserted here.
 *
 * PERSPECTIVAL GAP:
 *   Seats compute differently by construction. Female-category athletes and trapped service users sit at the target end: the rule reaches them through zero-sum domains they cannot exit. Trans women hold a genuinely dual position: the rule is their recognition guarantee and, because enforcement is privatized into individual access claims, a recurring personal cost center. Identity lock binds them to the dispute, since their category claim is their self-concept and exit equals renunciation, which pulls their derived directionality target-ward despite net-beneficiary status; if recognition were ever delivered through a channel decoupled from the contested category, the dual seat would decompose and the beneficiary-side extraction would surface plainly. Advocacy organizations and equality regulators sit at the beneficiary-administrator end and experience the arrangement as coordination they built. Excluded dissenters experience pure suppression: the arrangement's enforcement touches them without their participation. Coalition potential among the payer seats exists on paper, since athletes, service users, and dissenters share targets, but it is fragmented by differing concerns and mutual distrust, and that fragmentation is itself load-bearing for the arrangement's persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary and victim declarations drive the derivation. Transgender_women appear in both arrays, formal inclusion plus privatized enforcement burden, with identity_locked exit, landing them mid-range with a target-ward pull. Lgbt_advocacy_organizations are pure beneficiaries with mobile exit, landing near the subsidy end. Female_category_athletes and sex_segregated_service_users are declared victims with constrained and trapped exits respectively, landing near the full-target end, with trapped service users furthest. Equality_regulators and sports_governing_bodies administer the rule and collect remit and stability, tilting beneficiary-ward. Sex_based_rights_dissenters are excluded yet bear sanction costs, placing them target-ward despite never entering the arrangement. The analytical seat collects nothing. Scope amplification applies engine-side: the arrangement's continental-to-global reach makes verification of edge-case admissions harder, modestly amplifying effective extraction on the target seats. No directionality_overrides are authored: the dual-role and excluded positions are expressed through secondary_role and role declarations that the derivation chain reads, and the per-power-atom override key cannot distinguish the two moderate-power seats that would need different d values.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings. Read as pure coordination, the arrangement's zero-sum domains disappear and the athletes' and detainees' costs vanish into a liberation narrative. Read as pure extraction, the genuine recognition function disappears, erasing the ungated document pathway that ended psychiatric gatekeeping along with its real beneficiaries. Tangled rope holds both: coordination and asymmetric extraction through one structure that requires active enforcement to hold. Mandatrophy is not resolved: the founding problem of recognition denial is corroborated historically, but its present status is disputed, and the mismatch consumer reads status=contested against verdict=world_rearranges, which flags neither zombie nor resolved. The piton test fails clearly: concentrated beneficiaries exist in advocacy mandate and regulatory remit, so theatrical maintenance is not the load-bearing structure; the theater_ratio of 0.28 is symptom, not cause.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position,
    'This story instantiates one reading (gender_identity_reading) of the contested kernel woman_category; what would the sibling readings (sex_biology_reading, intersex_accommodation_reading) change structurally if adopted instead?',
    'Compile and classify the sibling stories; compare victim sets, directionality distributions, and per-seat types across the kernel family rather than adjusting anything within this story.',
    'Under the sex_biology_reading the victim set becomes transgender women, excluded from the category, and the sport and placement extraction inverts; under the intersex_accommodation_reading the victim set narrows to intersex people misclassified by binary biology. The structural delta lives in the cross-reading comparison, not in this file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_position, conceptual, 'Committer-frame routing: this constraint is one reading of the woman_category kernel, with sibling readings held as separate constraints.').

omega_variable(
    domain_epsilon_gradient_decomposition,
    'Measured through identity-document policy the arrangement''s epsilon sits near 0.45; through elite-sport eligibility and sex-segregated placement it sits near 0.75. Does the single-story aggregate of 0.62 mask two or three structurally distinct constraints sharing one definitional premise?',
    'Author per-domain sibling stories (identity documents, sport eligibility, segregated-service placement) and test whether their failure modes, enforcement machinery, and stakeholder sets diverge enough to satisfy the epsilon-invariance split.',
    'If they split, this aggregate''s classification is a weighted artifact; per-domain types, likely rope-flavored for documents and tangled-rope-to-snare-flavored for sport and placement, become the corpus units and this file becomes a family index.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_epsilon_gradient_decomposition, conceptual, 'Whether the domain gradient within this reading demands decomposition into per-domain constraint stories.').

omega_variable(
    enforcement_cost_incidence,
    'Who ultimately bears the cost of making recognition real: the institutions that adopted the rule, or the trans individuals whose individual access claims carry it?',
    'Track complaint, litigation, and appeal volumes by initiating party; sample service-admission disputes to determine whether institutions proactively implement recognition or require individuals to trigger each event.',
    'If enforcement is privatized onto the beneficiary class, the arrangement''s extraction concentrates on the people it nominally protects, strengthening the tangled-rope reading and predicting attrition in the constituency; if institutionalized, extraction shifts to institutional budgets and the beneficiary seat''s derived directionality drops.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_cost_incidence, empirical, 'Incidence of the arrangement''s boundary-defense costs: institutional versus privatized onto trans individuals.').

omega_variable(
    elite_sport_entry_trajectory,
    'Does women''s elite-sport eligibility converge on identity-plus-performance-criteria, revert to biology-referenced categories, or fragment by federation?',
    'Federation rule revisions and litigation outcomes over the next competitive cycle across the major Olympic federations and collegiate frameworks.',
    'Convergence on hedged criteria caps the sports-domain epsilon near 0.6 and stabilizes the aggregate; reversion pushes the sports domain toward the sex_biology_reading''s structure and raises this reading''s aggregate epsilon; fragmentation produces jurisdictional arbitrage and keeps epsilon volatile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(elite_sport_entry_trajectory, empirical, 'Trajectory of the highest-extraction domain under this reading.').

omega_variable(
    suppression_structural_vs_chilled,
    'Is the measured suppression of 0.66 carried by structural enforcement such as statutory mandates, employment sanction, and platform moderation, or by internalized self-censorship among would-be dissenters?',
    'Post-exit speech trajectories: if individuals who leave sanctioned environments resume biology-referenced argument freely, the suppression was structural; if they persist in avoidance after exit, a chilled-internalized component dominates.',
    'If substantially internalized, effective suppression exceeds the structural measure and survives any policy reversal, changing how much enforcement machinery the arrangement actually needs to hold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_chilled, empirical, 'Structural versus internalized (chilled) component of the arrangement''s suppression.').

omega_variable(
    hybrid_framework_persistence,
    'Medical-gatekeeper recognition schemes persist in several jurisdictions alongside pure self-ID regimes. Are these hybrids stable intermediate readings of the kernel, or transitional decay toward one pole?',
    'Longitudinal tracking of hybrid jurisdictions across successive revision cycles: do they tighten toward evidence thresholds or relax toward bare declaration?',
    'Stable hybrids would constitute a fourth reading the current sibling set omits, weakening the foreclosure typing at the fragment level; decay toward a pole confirms the pure readings as the only attractors.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(hybrid_framework_persistence, conceptual, 'Status of hybrid recognition schemes relative to the pure readings of the kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__gender_identity_reading, 0, 24).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woman_cat_gender_identity_tr_t0, woman_category__gender_identity_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(woman_cat_gender_identity_tr_t4, woman_category__gender_identity_reading, theater_ratio, 4, 0.12).
narrative_ontology:measurement(woman_cat_gender_identity_tr_t8, woman_category__gender_identity_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(woman_cat_gender_identity_tr_t12, woman_category__gender_identity_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(woman_cat_gender_identity_tr_t16, woman_category__gender_identity_reading, theater_ratio, 16, 0.24).
narrative_ontology:measurement(woman_cat_gender_identity_tr_t20, woman_category__gender_identity_reading, theater_ratio, 20, 0.27).
narrative_ontology:measurement(woman_cat_gender_identity_tr_t24, woman_category__gender_identity_reading, theater_ratio, 24, 0.28).

% Extraction over time
narrative_ontology:measurement(woman_cat_gender_identity_be_t0, woman_category__gender_identity_reading, base_extractiveness, 0, 0.18).
narrative_ontology:measurement(woman_cat_gender_identity_be_t4, woman_category__gender_identity_reading, base_extractiveness, 4, 0.26).
narrative_ontology:measurement(woman_cat_gender_identity_be_t8, woman_category__gender_identity_reading, base_extractiveness, 8, 0.38).
narrative_ontology:measurement(woman_cat_gender_identity_be_t12, woman_category__gender_identity_reading, base_extractiveness, 12, 0.5).
narrative_ontology:measurement(woman_cat_gender_identity_be_t16, woman_category__gender_identity_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement(woman_cat_gender_identity_be_t20, woman_category__gender_identity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement(woman_cat_gender_identity_be_t24, woman_category__gender_identity_reading, base_extractiveness, 24, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(woman_cat_gender_identity_su_t0, woman_category__gender_identity_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement(woman_cat_gender_identity_su_t4, woman_category__gender_identity_reading, suppression_requirement, 4, 0.3).
narrative_ontology:measurement(woman_cat_gender_identity_su_t8, woman_category__gender_identity_reading, suppression_requirement, 8, 0.42).
narrative_ontology:measurement(woman_cat_gender_identity_su_t12, woman_category__gender_identity_reading, suppression_requirement, 12, 0.52).
narrative_ontology:measurement(woman_cat_gender_identity_su_t16, woman_category__gender_identity_reading, suppression_requirement, 16, 0.6).
narrative_ontology:measurement(woman_cat_gender_identity_su_t20, woman_category__gender_identity_reading, suppression_requirement, 20, 0.64).
narrative_ontology:measurement(woman_cat_gender_identity_su_t24, woman_category__gender_identity_reading, suppression_requirement, 24, 0.66).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_category__gender_identity_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who counts as a woman' decomposes into three readings of one kernel, each with its own epsilon, victim set, and classification. This file instantiates the gender_identity_reading only. The sex_biology_reading is the inherited upstream default (high usage-frequency confidence); this reading is downstream of it and contests its criterion; the intersex_accommodation_reading is a biology-broadening refinement. A further decomposition axis is flagged but deferred: the domain gradient (identity documents near 0.45 versus elite sport and segregated placement near 0.75) may satisfy the epsilon-invariance split into per-domain stories sharing this definitional premise; see omega domain_epsilon_gradient_decomposition.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
