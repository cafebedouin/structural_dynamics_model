% ============================================================================
% CONSTRAINT STORY: woman_female_category__gender_identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_female_category__gender_identity_reading, []).

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
 *   constraint_id: woman_female_category__gender_identity_reading
 *   human_readable: Gender Self-Identification Criterion for the Woman/Female Category (Gender-Identity Reading)
 *   domain: political_philosophy/bioethics/gender_studies/law
 *
 * SUMMARY:
 *   The woman/female category's membership criterion is a contested kernel
 *   with three live readings; this story authors ONE of them -- the
 *   gender-identity reading: internal self-identification determines
 *   membership, independent of biological sex. As instantiated in
 *   self-identification statutes, equality-body guidance, and institutional
 *   dignity policies, the criterion replaces medicalized gatekeeping with a
 *   self-reported determinant that any institution can apply. The constraint
 *   has a genuine coordination function -- a determinate, non-intrusive
 *   criterion for a category that law, services, sport, and social life all
 *   require -- and it extracts asymmetrically: concentrated contest and
 *   backlash costs fall on the transgender seat the criterion protects,
 *   concentrated fairness costs fall on natal female athletes where it
 *   governs competition, diffuse definitional-contest costs fall on all
 *   category members, and suppression of the sibling determinants
 *   (biological, context-partitioned) is maintained by institutional
 *   discipline and social sanction. Constraint family note: this epsilon is
 *   authored in dignity/recognition currency over the fixed referent (the
 *   standing contested arrangement) by this reading's own lights; the sibling
 *   stories author epsilon for the same referent in their own currencies and
 *   reach different values -- the divergence is reading-indexed valuation
 *   over a fixed referent, not measurement instability, and the family is
 *   linked via network.affects_constraints.
 *
 * KEY AGENTS:
 *   - transgender_individuals: Primary beneficiary and simultaneous payer (moderate/identity_locked) -- collects recognition, access, and dignity protection; bears the contest's heaviest scrutiny and backlash costs
 *   - self_identified_women: Category members bearing diffuse definitional-contest extraction (moderate/identity_locked) -- the victim set as this reading defines it, trans women included
 *   - natal_female_athletes: Concentrated-cost payers in competition domains (moderate/constrained) -- bear fairness costs without setting the rule
 *   - legislative_judicial_bodies: Agenda-setter (institutional/arbitrage) -- adopts, administers, and adjudicates the criterion; arbitrages between determinants context by context
 *   - gender_identity_advocacy_orgs: Secondary beneficiary (organized/mobile) -- collects mandate, standing, and funding from the criterion's adoption
 *   - gender_critical_campaigners: Suppression-bearing payers (organized/constrained) -- assert the sibling determinants and absorb the discipline and sanction the criterion's enforcement applies
 *   - analytical_observers: Analytical seat (analytical/analytical) -- no extraction, no rents; produces the classification
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, 0.66).
domain_priors:suppression_score(woman_female_category__gender_identity_reading, 0.58).
domain_priors:theater_ratio(woman_female_category__gender_identity_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(woman_female_category__gender_identity_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_female_category__gender_identity_reading, tangled_rope).
narrative_ontology:human_readable(woman_female_category__gender_identity_reading, "Gender Self-Identification Criterion for the Woman/Female Category (Gender-Identity Reading)").
narrative_ontology:topic_domain(woman_female_category__gender_identity_reading, "political_philosophy/bioethics/gender_studies/law").

domain_priors:requires_active_enforcement(woman_female_category__gender_identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_female_category__gender_identity_reading, 'f6294772-fdd2-43d5-9ce2-498a173f2c7c').
narrative_ontology:cs_kernel_codification('f6294772-fdd2-43d5-9ce2-498a173f2c7c', distributed).
narrative_ontology:cs_authority_grounding('f6294772-fdd2-43d5-9ce2-498a173f2c7c', self_enforcing).
narrative_ontology:cs_reading_relation('f6294772-fdd2-43d5-9ce2-498a173f2c7c', woman_female_category__sex_biology_reading, forecloses).
narrative_ontology:cs_reading_relation('f6294772-fdd2-43d5-9ce2-498a173f2c7c', woman_female_category__hybrid_contextual_reading, forecloses).
narrative_ontology:cs_axiom('f6294772-fdd2-43d5-9ce2-498a173f2c7c', foundational, self_identification_is_authoritative).
narrative_ontology:cs_axiom_status(self_identification_is_authoritative, holdable).
narrative_ontology:cs_axiom_grounding('f6294772-fdd2-43d5-9ce2-498a173f2c7c', self_identification_is_authoritative, deontological).
narrative_ontology:cs_axiom('f6294772-fdd2-43d5-9ce2-498a173f2c7c', foundational, category_independence_from_biology).
narrative_ontology:cs_axiom_status(category_independence_from_biology, holdable).
narrative_ontology:cs_axiom_grounding('f6294772-fdd2-43d5-9ce2-498a173f2c7c', category_independence_from_biology, conventional).
narrative_ontology:cs_reference_frame('f6294772-fdd2-43d5-9ce2-498a173f2c7c', identity_governed_category_membership).
narrative_ontology:cs_drift_state('f6294772-fdd2-43d5-9ce2-498a173f2c7c', contemporary, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f6294772-fdd2-43d5-9ce2-498a173f2c7c', '').
narrative_ontology:cs_kernel_id(woman_female_category__gender_identity_reading, woman_female_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, self_identified_women).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, natal_female_athletes).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_female_category__gender_identity_reading, gender_identity_advocacy_orgs).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:constraint_victim(woman_female_category__gender_identity_reading, gender_critical_campaigners).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, gender_self_determination_doctrine).
narrative_ontology:constraint_vindicates(woman_female_category__gender_identity_reading, sex_gender_conceptual_distinction).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain a recognition claim that requires no medical evidence, diagnosis, or third-party attestation: legal status change by declaration, access to female-category spaces, services, and competitive categories consistent with identity, and institutional dignity rules enforced by policy. The same arrangement makes them the contest's central figures: their identity claims are publicly adjudicated in litigation and media, they bear backlash cycles when policies reverse, and they carry the burden of representing the category's boundary in every contested domain. Exit is unavailable without abandoning the identity claims the criterion exists to protect.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, transgender_individuals, beneficiary,
    moderate, biographical, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(woman_female_category__gender_identity_reading, transgender_individuals, payer).

% The category's members as this reading defines them -- anyone identifying as a woman, trans women included. They bear the diffuse costs of the category operating as a contested battlefield: heightened definitional scrutiny of their own membership, conscription into boundary disputes they did not start, and the standing arrangement's dignity extraction wherever recognition or category-consistent treatment in spaces and services is withheld or litigated. Exit from the category is unavailable under any reading of the kernel; the contest follows the category, not any member's choice.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, self_identified_women, payer,
    moderate, biographical, identity_locked, global).

% Compete in female categories whose membership is set by self-identification under this reading. Where the criterion governs competition, they bear concentrated performance-fairness costs: category records, podium places, and roster places shift with identity-based inclusion. They do not set the rule; their exit -- leaving elite competition -- forfeits the careers the category structure exists to organize.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, natal_female_athletes, payer,
    moderate, biographical, constrained, global).

% Adopt, administer, and adjudicate the criterion: self-identification statutes, equality-body guidance, tribunal rulings on single-sex space exceptions, and sports-category regulations. They can move between determinants context by context -- the arbitrage that produces hybrid arrangements -- and bear the political cost of whichever determinant they pick or withhold. Their docket is the contest's official record.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, legislative_judicial_bodies, agenda_setter,
    institutional, generational, arbitrage, national).

% Obtain mandate, standing, and funding from the criterion's adoption: they draft institutional guidance, train compliance staff, intervene in litigation, and administer dignity-complaint processes. Their organizational purpose is bound to the criterion's spread; they can pivot missions but at the cost of institutional identity.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_identity_advocacy_orgs, beneficiary,
    organized, generational, mobile, global).

% Assert the sibling determinants -- biological or context-partitioned criteria for the category -- and bear the constraint's suppression directly: institutional discipline, exclusion from some professional and standard-setting venues, social sanction, and in some jurisdictions legal exposure for noncompliance with dignity rules. Their position is defined against the criterion; exit would dissolve the position itself. They retain strong channels in public discourse, electoral politics, and litigation even where excluded from the bodies that write category rules.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, gender_critical_campaigners, payer,
    organized, generational, constrained, global).

% Philosophers, bioethicists, and comparative jurists tracking what the category's determinant does to adjacent structures -- sex-based provisions, medical categorization, sports governance, legal personhood. They bear no extraction and collect no rents; their output is the classification the corpus exists to take.
narrative_ontology:constraint_stakeholder(woman_female_category__gender_identity_reading, analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_female_category__gender_identity_reading, transgender_individuals).
narrative_ontology:fixing_cost_class(woman_female_category__gender_identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies one determinate, non-intrusive criterion for who counts as a woman/female across legal recognition, single-sex services and spaces, competitive categories, and social categorization -- replacing case-by-case biological or medical gatekeeping (diagnosis, anatomy verification, committee adjudication) with a self-reported determinant any institution can apply without verifying anyone's body, and giving transgender people a recognition claim that does not depend on medical evidence or third-party attestation.
% TRANSFER_FUNCTION: Moves recognition and access -- legal status, female-category spaces and services, competitive categories, dignity-rule enforcement -- to anyone self-identifying with the category; moves the costs of administering and defending that criterion (compliance burdens, contest and backlash cycles, sanction enforcement, concentrated fairness costs in competition) onto category members, service operators, athletes, and dissenting institutions.
% ABSENT_VOICES: Natal female service users in shelters and prisons, female athletes excluded from some federations' category-rule consultations, and gender-critical scholars no-platformed from some institutional venues would object that the criterion's costs were never priced with them in the room; their objections surface in litigation, electoral politics, and public discourse rather than in the standard-setting processes where the criterion was adopted.
% DISAPPEARANCE_RATIONALE: Legal gender recognition would revert to medicalized gatekeeping; institutions would re-erect biological or committee-based criteria for spaces, services, and sport; the advocacy infrastructure would lose its mandate; and the contest would reorganize around the re-imposed gatekeeping rather than dissolving. Every named seat's arrangements depend on the criterion -- as protection (transgender seat), as burden (women's and athletes' seats), as docket (agenda-setter seat), as mandate (advocacy seat).
% FOUNDING_PROBLEM: Medicalized gender recognition imposed gatekeeping: transgender people had to satisfy diagnosis requirements, sometimes sterilization or medical-transition requirements, and committee approval to have their category membership recognized; purely biological criteria also misclassified intersex people and handed institutions intrusive verification power over intimate identity.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated outside the beneficiary set: statutory repeal findings and legislative records document the prior diagnosis and sterilization requirements; intersex advocates' testimony documents nonconsensual biological classification; and the reading's opponents concede the historical existence of gatekeeping harms while disputing the adopted remedy. No corroborating source disputes that the founding problem existed -- the dispute is over whether self-identification solves it or trades it for new harms in competition and single-sex services.
narrative_ontology:disappearance_verdict(woman_female_category__gender_identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_female_category__gender_identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_female_category__gender_identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_female_category__gender_identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_female_category__gender_identity_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_female_category__gender_identity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_female_category__gender_identity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_female_category__gender_identity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.66: the standing arrangement extracts in dignity/recognition currency (misrecognition, scrutiny, and backlash borne substantially by the protected seat itself), plus concentrated fairness costs in competitive domains, plus the enforcement burden of holding the criterion against the sibling determinants -- net of a genuine coordination function that a pure-extraction reading would miss. Suppression 0.58: structural (institutional discipline, dignity-complaint processes, some legal exposure for noncompliance) rather than carceral; it suppresses the sibling determinants, not persons. Theater 0.28: the criterion's functions are mostly real; a symbolic compliance layer (training rituals, declarations) grows as adoption matures. Accessibility_collapse 0.30: the alternatives -- the biological and hybrid determinants -- remain vigorously live in courts, federations, and legislatures; understanding this constraint does not collapse them. Resistance 0.72: litigation, policy reversals, sports-governance carve-outs, and grassroots campaigns contest the criterion continuously. Claim/metric independence: claimed_type tangled_rope is authored from the structural analysis (genuine coordination + asymmetric extraction + active enforcement required); the metrics are authored independently from the arrangement's observed operation; the engine computes per-seat classifications from the structural data, and divergence between claim and computed type is signal, not error. Measurements share one grid (t=0 to 30 at steps of 5, abstract institutional time spanning the self-identification adoption era to present contestation maturity); trajectories are monotonic rising -- extraction accumulation as adoption widens the contest surface, suppression requirement rising as enforcement machinery matures against growing resistance, no cyclical claim. Receipt surface: the gains demonstrably accrue to the transgender seat (the criterion's product is their recognition and access), with advocacy organizations collecting secondary mandate gains; fixing_cost is prohibitive -- removal or context-partitioning would require the agenda-setter to re-litigate the kernel itself, the political cost is extreme, and by this reading's lights the benefit of removal is negative (it would restore the gatekeeping harms).
 *
 * PERSPECTIVAL GAP:
 *   The seats compute different constraints from the same structure. From the transgender seat the criterion is dignity-protecting coordination whose extraction arrives as contest costs it cannot decline -- recognition and burden land together on the same agents. From the natal-female-athlete seat the same criterion is concentrated extraction in fairness currency, imposed without agenda power and exitable only by leaving elite sport. From the agenda-setter seat it is an administrative determinant with a political price attached to every context it is applied to or withheld from. From the advocacy seat it is mandate. The engine computes these per-seat classifications from power, exit, and role data; the divergence is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (transgender_individuals; advocacy organizations) derive low directionality for those seats -- the criterion subsidizes them. Victim declarations (self_identified_women; natal_female_athletes) derive high directionality. Two structural complications: the transgender seat is dual-positioned (beneficiary role, payer secondary role) with identity_locked exit, which pulls its derived directionality up from the beneficiary end toward the middle, because recognition gains and contest burdens land on the same agents; and the women's seats carry identity_locked or constrained exit, which amplifies their effective extraction toward the full-target end relative to mobile agents at equal power. Global spatial scope amplifies effective extraction modestly for all seats. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled. No directionality_overrides are authored: the override array keys on power atom alone, and this story's three distinct moderate-power seats (transgender individuals, self-identified women, natal female athletes) would collide on any single override; the beneficiary/victim declarations plus exit atoms already differentiate them.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem -- medicalized gatekeeping of recognition -- is live wherever the criterion is not adopted, so no mandatrophy is declared, and the R5 mismatch consumer sees status=live with verdict=world_rearranges: consistent, no zombie flag. The tangled_rope claim does the anti-mislabel work in both directions. Against the pure-rope collapse: this reading's own lights could present the criterion as pure dignity coordination, but the concentrated domain costs and the enforcement requirement keep real extraction on the books. Against the pure-snare collapse: the sibling reading reads the same structure as category capture, but the coordination function -- a determinate, non-intrusive criterion that every institution operating a gendered category needs -- is real and would have to be rebuilt if the criterion vanished. The kernel decomposition is what keeps these two mislabels apart: each reading authors its own epsilon over the shared referent instead of one observable-dependent story oscillating between them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_instantiation,
    'Which determinant of the woman/female category does a given jurisdiction''s standing arrangement actually instantiate -- this reading''s self-identification determinant, the sibling biology determinant, or the sibling context-partition -- given that this story authors only the gender-identity reading?',
    'Jurisdiction-level comparative classification across the three sibling constraint stories, keyed on what each jurisdiction''s statutes, tribunal rulings, and sports regulations actually use as the determinant.',
    'The victim set and the currency of epsilon flip with the determinant: a jurisdiction running biological or hybrid arrangements instantiates a sibling constraint, not this one, and this story''s victim set and extraction profile do not transfer to it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer structure: this constraint is the gender-identity reading of kernel woman_female_category; sibling readings are separate constraints.').

omega_variable(
    epsilon_currency_indexicality,
    'Is the divergence between this story''s epsilon (authored in dignity/recognition currency by this reading''s lights) and the sibling stories'' epsilon for the same standing referent fully explained by reading-indexed valuation over a fixed referent, or does any part of the arrangement itself shift under different determinants?',
    'Cross-reading decomposition of the three family stories'' epsilon components over the shared referent; if the components differ only by valuation weights, the referent is fixed; if structural components differ, the readings describe different arrangements.',
    'If the arrangement itself shifts, the three stories measure different things, the family''s network links weaken, and cross-reading epsilon comparison becomes invalid rather than informative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_currency_indexicality, conceptual, 'Whether reading-indexed epsilon divergence reflects valuation over a fixed referent or genuinely different arrangements.').

omega_variable(
    suppression_mechanism_split,
    'Is the measured suppression structural (institutional discipline, dignity-complaint processes, legal exposure for noncompliance) or internalized (self-censorship and social-pressure compliance that would persist if the rules were withdrawn)?',
    'Track formal sanction volumes against voluntary-compliance trajectories after policy adoption; if compliance persists where enforcement capacity is removed, the internalized share is large.',
    'If the internalized share is large, the constraint''s effective suppression exceeds the structural measure, and removal costs shift from repealing rules to unwinding internalized enforcement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized share of the suppression that holds the criterion against sibling determinants.').

omega_variable(
    concentrated_domain_cost_magnitude,
    'How large are the concentrated costs the criterion imposes in competitive sport and single-sex services, assessed on this reading''s own terms -- the reading holds them outweighed by the dignity stakes, but their magnitude is empirically open?',
    'Sports-governance performance and record data with adequate controls; service-outcome studies from jurisdictions with mature self-identification adoption.',
    'Large magnitudes grow the extraction component of this tangled_rope and strain the reading''s outweighing judgment; small magnitudes push the constraint toward rope and vindicate the reading''s cost assessment.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(concentrated_domain_cost_magnitude, empirical, 'Magnitude of the concentrated fairness and service costs this reading acknowledges and holds outweighed.').

omega_variable(
    dual_position_stability,
    'The transgender seat is dual-positioned -- primary beneficiary of the criterion and simultaneously bearer of the contest''s heaviest dignity and backlash costs. Does maturing adoption resolve the payer side (normalization) or intensify it (backlash cycles)?',
    'Longitudinal dignity-harm and backlash-incident data across adoption cohorts, comparing early-adopting and late-adopting jurisdictions at matched years post-adoption.',
    'If the payer side resolves, the seat''s derived directionality falls toward the beneficiary end and extraction concentrates elsewhere; if it intensifies, the dual position hardens and the tangled structure deepens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dual_position_stability, empirical, 'Whether the beneficiary seat''s simultaneous payer burden persists, resolves, or grows with adoption maturity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_female_category__gender_identity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(woma_tr_t0, woman_female_category__gender_identity_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(woma_tr_t0, observed).
narrative_ontology:measurement(woma_tr_t5, woman_female_category__gender_identity_reading, theater_ratio, 5, 0.15).
narrative_ontology:measurement_basis(woma_tr_t5, observed).
narrative_ontology:measurement(woma_tr_t10, woman_female_category__gender_identity_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(woma_tr_t10, observed).
narrative_ontology:measurement(woma_tr_t15, woman_female_category__gender_identity_reading, theater_ratio, 15, 0.21).
narrative_ontology:measurement_basis(woma_tr_t15, observed).
narrative_ontology:measurement(woma_tr_t20, woman_female_category__gender_identity_reading, theater_ratio, 20, 0.24).
narrative_ontology:measurement_basis(woma_tr_t20, observed).
narrative_ontology:measurement(woma_tr_t25, woman_female_category__gender_identity_reading, theater_ratio, 25, 0.26).
narrative_ontology:measurement_basis(woma_tr_t25, observed).
narrative_ontology:measurement(woma_tr_t30, woman_female_category__gender_identity_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(woma_tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(woma_be_t0, woman_female_category__gender_identity_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement_basis(woma_be_t0, observed).
narrative_ontology:measurement(woma_be_t5, woman_female_category__gender_identity_reading, base_extractiveness, 5, 0.45).
narrative_ontology:measurement_basis(woma_be_t5, observed).
narrative_ontology:measurement(woma_be_t10, woman_female_category__gender_identity_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(woma_be_t10, observed).
narrative_ontology:measurement(woma_be_t15, woman_female_category__gender_identity_reading, base_extractiveness, 15, 0.55).
narrative_ontology:measurement_basis(woma_be_t15, observed).
narrative_ontology:measurement(woma_be_t20, woman_female_category__gender_identity_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(woma_be_t20, observed).
narrative_ontology:measurement(woma_be_t25, woman_female_category__gender_identity_reading, base_extractiveness, 25, 0.63).
narrative_ontology:measurement_basis(woma_be_t25, observed).
narrative_ontology:measurement(woma_be_t30, woman_female_category__gender_identity_reading, base_extractiveness, 30, 0.66).
narrative_ontology:measurement_basis(woma_be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(woma_su_t0, woman_female_category__gender_identity_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(woma_su_t0, observed).
narrative_ontology:measurement(woma_su_t5, woman_female_category__gender_identity_reading, suppression_requirement, 5, 0.39).
narrative_ontology:measurement_basis(woma_su_t5, observed).
narrative_ontology:measurement(woma_su_t10, woman_female_category__gender_identity_reading, suppression_requirement, 10, 0.44).
narrative_ontology:measurement_basis(woma_su_t10, observed).
narrative_ontology:measurement(woma_su_t15, woman_female_category__gender_identity_reading, suppression_requirement, 15, 0.49).
narrative_ontology:measurement_basis(woma_su_t15, observed).
narrative_ontology:measurement(woma_su_t20, woman_female_category__gender_identity_reading, suppression_requirement, 20, 0.53).
narrative_ontology:measurement_basis(woma_su_t20, observed).
narrative_ontology:measurement(woma_su_t25, woman_female_category__gender_identity_reading, suppression_requirement, 25, 0.56).
narrative_ontology:measurement_basis(woma_su_t25, observed).
narrative_ontology:measurement(woma_su_t30, woman_female_category__gender_identity_reading, suppression_requirement, 30, 0.58).
narrative_ontology:measurement_basis(woma_su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_female_category__gender_identity_reading, identity_coordination).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__sex_biology_reading).
narrative_ontology:affects_constraint(woman_female_category__gender_identity_reading, woman_female_category__hybrid_contextual_reading).

% DUAL FORMULATION NOTE:
% The colloquial question 'what determines membership in the woman/female category' decomposes into three structurally distinct constraints: this story (gender-identity reading -- identity determines, independent of biology), woman_female_category__sex_biology_reading (biology determines), and woman_female_category__hybrid_contextual_reading (the determinant varies by context). Each carries its own epsilon, victim set, and classification; epsilon differs across readings because the determinant claim differs, not because the arrangement is measured differently. The sex-biology reading is upstream (the traditional institutional default); this reading's spread created the structural pressure from which the hybrid reading emerged as a context-partitioning compromise. Sibling stories must carry reciprocal links and this note.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
