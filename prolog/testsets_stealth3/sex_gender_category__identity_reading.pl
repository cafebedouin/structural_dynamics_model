% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-14
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
 *   human_readable: Gender Self-Identification Category Membership Rule (Identity Reading)
 *   domain: social/legal ontology
 *
 * SUMMARY:
 *   This constraint is the identity reading of the contested sex-gender
 *   classification kernel: category membership determined by subjective
 *   gender identity, operationalized as self-declaration in statutes,
 *   registries, institutional policy, and space-access rules. The arrangement
 *   solves a real classification and dignity problem for people whose
 *   identity diverges from recorded sex, and it does so by transferring
 *   definitional authority away from anatomical and medical determinants — a
 *   transfer whose costs land unevenly on cis women's exclusive claim, on
 *   athletes in female categories, on dissenters sanctioned for holding
 *   alternative criteria, and on the displaced gatekeeping professions.
 *   Enforcement is cheap at the boundary (no tests to run) but expensive in
 *   the surround: compliance mandates, speech norms, and litigation. The
 *   claim/metric gap is deliberate and independent: the arrangement is
 *   CLAIMED here as tangled_rope on structural grounds (genuine coordination
 *   function plus asymmetric extraction through the same structure plus
 *   active enforcement), while the metrics are authored from the identity
 *   reading's own lights over the fixed referent of the standing arrangement
 *   — the engine measures any divergence.
 *
 * KEY AGENTS:
 *   - transgender_people: Primary beneficiary (powerless/trapped) — recognition path runs wholly through the rule
 *   - trans_rights_advocacy_networks: Agenda-setting beneficiary (organized/identity_locked) — authored and propagates the criterion
 *   - cis_women: Dual-positioned class seat (powerful/constrained) — retains membership, bears dilution costs in exposed spaces
 *   - female_category_athletes: Competition-domain target (moderate/constrained)
 *   - sex_classification_dissenters: Sanctioned holders of alternative criteria (organized/constrained)
 *   - clinical_gatekeeping_professions: Displaced gatekeepers redeploying into care (institutional/arbitrage)
 *   - civil_registry_administrators: Administrator/agenda setter (institutional/mobile)
 *   - women_sports_governing_bodies: Sectoral boundary administrator contesting or adopting the criterion (institutional/mobile)
 *   - diversity_compliance_industry: Receipt seat for the compliance apparatus (organized/mobile)
 *   - comparative_classification_analysts: Analytical observer (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.32).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.55).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Gender Self-Identification Category Membership Rule (Identity Reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social/legal ontology").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '03a58576-c04b-4163-851c-06e0a3e09089').
narrative_ontology:cs_kernel_codification('03a58576-c04b-4163-851c-06e0a3e09089', distributed).
narrative_ontology:cs_authority_grounding('03a58576-c04b-4163-851c-06e0a3e09089', distributed).
narrative_ontology:cs_reading_relation('03a58576-c04b-4163-851c-06e0a3e09089', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('03a58576-c04b-4163-851c-06e0a3e09089', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('03a58576-c04b-4163-851c-06e0a3e09089', foundational, self_identified_identity_constitutes_category_membership).
narrative_ontology:cs_axiom_status(self_identified_identity_constitutes_category_membership, holdable).
narrative_ontology:cs_axiom_grounding('03a58576-c04b-4163-851c-06e0a3e09089', self_identified_identity_constitutes_category_membership, deontological).
narrative_ontology:cs_axiom('03a58576-c04b-4163-851c-06e0a3e09089', secondary, no_medical_certification_required_for_recognition).
narrative_ontology:cs_axiom_status(no_medical_certification_required_for_recognition, holdable).
narrative_ontology:cs_axiom_grounding('03a58576-c04b-4163-851c-06e0a3e09089', no_medical_certification_required_for_recognition, instrumental).
narrative_ontology:cs_reference_frame('03a58576-c04b-4163-851c-06e0a3e09089', self_declared_identity_as_membership_ground).
narrative_ontology:cs_drift_state('03a58576-c04b-4163-851c-06e0a3e09089', contemporary_codification_and_backlash_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('03a58576-c04b-4163-851c-06e0a3e09089', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, transgender_people).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, nonbinary_people).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_rights_advocacy_networks).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, diversity_compliance_industry).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, female_category_athletes).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, sex_classification_dissenters).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, clinical_gatekeeping_professions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, cis_women).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, gender_self_determination_principle).
narrative_ontology:constraint_vindicates(sex_gender_category__identity_reading, legal_recognition_reduces_minority_stress).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live with a felt identity that differs from their recorded sex at birth. Under this rule they obtain corrected documents, facility access, and social recognition by declaring their identity — no diagnosis, committee, or body inspection required. Their entire recognition path runs through the rule: if it were repealed or narrowed, they would fall back to medical gatekeeping many cannot access or afford, or to permanent documentary mismatch.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, transgender_people, beneficiary,
    powerless, biographical, trapped, national).

% Hold identities outside the man/woman binary. They benefit from the loosening of rigid anatomical classification — freer everyday presentation and fewer forced binaries where systems permit — but most registries still force an M or F marker, so the benefit is partial and unevenly implemented.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, nonbinary_people, beneficiary,
    powerless, biographical, constrained, national).

% Campaign coalitions that drafted, popularized, and lobbied the self-declaration standard across jurisdictions. They gain mission validation, membership, funding, and agenda influence from the rule's spread, and their organizations' identities are fused with its success — disowning it is practically unthinkable. Model statutes, talking points, and institutional guidance originate in these networks.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_rights_advocacy_networks, beneficiary,
    organized, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, trans_rights_advocacy_networks, agenda_setter).

% Members of the category the rule widens. They keep unconditional membership — no one disputes they belong — and keep most protections built on it. What changes is exclusivity: eligibility for women's spaces, services, prizes, and rosters now extends to anyone who declares a female identity, and in the most exposed settings (prisons, refuges, hospital wards, contact sport) they bear the resulting adjustment costs, complaint burdens, and contested conditions. Membership is ascribed to them regardless of preference; exiting the category is not available.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women, payer,
    powerful, biographical, constrained, global).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, cis_women, beneficiary).

% Compete in female-designated competitions whose eligibility criteria include declared identity in a growing set of federations and school systems. They absorb changed competitive conditions, altered results in some disciplines, and the public controversy surrounding each eligibility ruling; their alternatives are moving to whatever categories are offered, switching sports, or leaving competition.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, female_category_athletes, payer,
    moderate, biographical, constrained, global).

% People and organizations who maintain that membership is or should be determined by anatomy or by medically verified transition — gender-critical feminist groups, some conservative and religious organizations, some clinicians and academics. Where the rule operates they face professional discipline, platform restrictions, lost contracts, and social sanction; their alternative criterion is displaced from official documents and services and survives mainly in private association and unregulated domains.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, sex_classification_dissenters, payer,
    organized, biographical, constrained, global).

% Psychiatrists, endocrinologists, and clinic systems that formerly controlled classification through diagnosis and approval letters. The rule removes their gatekeeping role along with the associated assessment authority and revenue; many redeploy into delivering transition care itself. Professional bodies in some countries formally opposed losing the certification step.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, clinical_gatekeeping_professions, payer,
    institutional, biographical, arbitrage, national).

% Government offices and registrars that issue identity documents. They administer the rule: accept the declaration, update the record, process disputes. Operating cost is low for them — no verification apparatus to run — and they can amend forms, guidance, and with legislation the rule itself. They carry the arrangement's public-facing legitimacy.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, civil_registry_administrators, agenda_setter,
    institutional, generational, mobile, national).

% Federations that own eligibility rules for female categories. Several retain anatomy- or performance-based criteria and thereby decline the identity criterion inside their domain; others adopt inclusive policies. Either way they set the boundary in their sector, can move it, and absorb the litigation and diplomatic pressure that follows.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, women_sports_governing_bodies, agenda_setter,
    institutional, generational, mobile, global).

% Consultancies, trainers, auditors, and software vendors selling identity-inclusion compliance: policy templates, pronoun-system rollouts, sensitivity training, monitoring dashboards. They are paid out of the compliance activity the rule generates across employers and institutions, and their revenue depends on the arrangement continuing to produce obligations to satisfy.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, diversity_compliance_industry, beneficiary,
    organized, biographical, mobile, global).

% Scholars and legal comparativists tracking how jurisdictions operationalize the criterion, measuring outcomes in sport, incarceration, and service delivery, and mapping which membership criterion each statute actually instantiates. They neither collect nor pay under the rule; they observe and publish.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, comparative_classification_analysts, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, diversity_compliance_industry).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single, uniformly applicable rule for assigning category membership — self-declaration — replacing case-by-case adjudication of who counts as a woman or man. It coordinates document issuance, service allocation, and space assignment around one criterion and eliminates the need for institutions to run medical-verification procedures or interrogate applicants' bodies and histories.
% TRANSFER_FUNCTION: Moves definitional authority over the category from biological and medical determinants to individual declaration. Distributes access to the women's-category protections, spaces, and resources across a widened member set; shifts the enforcement burden from certifying gatekeepers onto institutions that must comply and dissenters who bear sanction for maintaining alternative criteria.
% ABSENT_VOICES: Users of the most exposed protected spaces — women in prisons, refuges, and hospital wards — were largely absent from the consultations that drafted self-declaration provisions in several jurisdictions. Gender-critical feminist organizations were excluded from a number of advisory panels and subsequently restricted from parts of the institutional conversation; their objections now arrive chiefly through litigation and press rather than design tables.
% DISAPPEARANCE_RATIONALE: Documents issued on declaration, access arrangements built on the criterion, and the compliance and advocacy economies grown around it would all require rework; people recognized under the rule would face reclassification; institutions would rebuild verification procedures. The underlying category system predates the rule and would persist, but its current operating configuration depends on the rule.
% FOUNDING_PROBLEM: Legal and social classification tied membership to anatomy and required trans people to obtain psychiatric diagnosis and, in many jurisdictions, sterilization or irreversible medical procedures before documents or recognition would be corrected — a gatekeeping regime producing documentary mismatch, exposure in routine encounters, and exclusion from identity-appropriate facilities.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by human-rights monitoring bodies (Council of Europe commissioner reporting, UN independent-expert filings) and by clinical literature documenting gatekeeping harms. Opponents of the identity criterion generally concede the historical gatekeeping problem while disputing this remedy — the disagreement is over the solution, not over whether the founding problem existed.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

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
 *   Time points map roughly onto the thirty years spanning pre-codification baseline (point 0) to the present (point 30); the codification wave begins mid-series. Scores are authored from the identity reading's own lights with the referent fixed on the standing self-declaration arrangement. Extractiveness 0.32: the reading regards the core operation as corrective coordination, and the extraction it acknowledges is enforcement overhead — compelled institutional compliance, sanction costs on dissenters, and the dilution costs its own structural delta records (cis women lose exclusive claim) — real but bounded, hence moderate rather than high. Suppression 0.55 is authored as a raw structural property and deliberately NOT scaled by power or scope: where the rule operates, the biological criterion is legally displaced from documents and services and dissent carries professional and platform sanction, while biomedical recording and private association persist outside the regulated surface. Theater 0.38: a growing performative layer (signaling statements, ritual training) rides atop a genuinely functional recognition core. Accessibility_collapse 0.60: alternatives collapse inside regulated domains but survive outside them — sporting bodies retaining eligibility rules, clinical sex recording, private associations — so understanding the constraint forecloses the sibling criteria only partially. Resistance 0.75: sustained litigation, mass mobilization, and institutional counter-administration meet the rule wherever it operates. All three temporal series share one seven-point grid (0,5,...,30) so no metric inherits another's end-state at earlier times, and each series' endpoint equals its base_properties value. The suppression_requirement series is authored because this story specifically traces enforcement-capacity build-out (speech-norm hardening, compliance mandates, disciplinary formalization), which rose steeply during codification and has begun plateauing. Receipt-surface notes: gain_flow names diversity_compliance_industry because the monetizable slice of the enforcement burden — purchased compliance — demonstrably accrues there, while sanction costs and dilution costs are borne, not received, by the payer seats; no other seat captures a concentrated share. fixing_cost is prohibitive: mechanically, repeal is a trivial amendment, but every attempted reversal on record has proven coalition-shattering, so the cost to whoever could fix it exceeds any plausible benefit they could bank.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the transgender_people seat the arrangement is nearly pure coordination: it delivers recognition at negligible personal cost, and repeal is the only catastrophe on the horizon. From the female_category_athletes and sex_classification_dissenters seats the same structure operates as enforced extraction: eligibility widened without consent, alternative criteria suppressed with sanction. The cis_women seat is genuinely dual — retained membership and most protections on one side, concentrated dilution costs in the most exposed settings on the other — which is why its directionality is overridden rather than derived from the victims list alone. Agenda-setting seats experience the constraint as administration: cheap to operate at the boundary, costly to defend in public. Same-level differentiation is visible among institutional actors: civil registry administrators and sports governing bodies hold identical power atoms but opposite relationships — one administers the criterion, the other frequently refuses it inside its own domain — differentiated by exit mobility and sectoral jurisdiction. The engine computes these divergences from the structural data; nothing in the authored claim adjudicates them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive transgender_people, nonbinary_people, trans_rights_advocacy_networks, and diversity_compliance_industry toward the subsidized end of d; victim declarations drive female_category_athletes, sex_classification_dissenters, and clinical_gatekeeping_professions toward the target end. Clinicians are included among victims deliberately: the arrangement extracts their classification authority and assessment revenue even though the reading endorses that removal — a clean illustration of reading-indexed epsilon sitting over structural victim data. cis_women carry the story's single directionality override (powerful -> 0.62): the derivation chain, seeing them in the victims list, would place them near the full-target end, but their true relationship is mixed — unconditional membership and most protections retained, costs concentrated in specific access domains — so the override corrects the derivation rather than replacing the structural data. Suppression is discussed unscaled throughout; only extractiveness is scaled by directionality and spatial scope in the engine's arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — coercive medical gatekeeping — remains live wherever the identity criterion has not been adopted, so the arrangement is not a mandatrophy corpse; its mandate tracks an unresolved problem. The live risks are drift-shaped rather than obsolescence-shaped: theater_ratio climbs with the compliance-signaling economy (the Goodhart vector, visible in the measurement series), and the enforcement-ratchet omega tracks whether suppression keeps hardening after codification plateaus. The mismatch consumer reads founding_problem_status=live against disappearance_verdict=world_rearranges and finds no capture/zombie flag — correctly, since the coordination function remains primary and no seat captures the extraction wholesale. If a future revision finds the founding problem solved across most operating jurisdictions while enforcement machinery continues growing, this story should be re-authored with status dead and the piton differential examined directly.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_split,
    'This constraint instantiates only the identity reading of the sex_gender_category kernel; the biology and hybrid readings are separate constraints with different membership criteria, victim sets, and extraction profiles — which criterion a jurisdiction adopts determines which constraint actually operates there.',
    'Per-jurisdiction statutory audit of the operative membership criterion across documents, spaces, and sport, mapped to reading identifiers; the sibling stories sex_gender_category__biology_reading and sex_gender_category__hybrid_reading carry the other instantiations and are linked through the network block.',
    'The classification computed here applies only where the identity criterion operates; under the biology reading the victim set contracts to trans people denied recognition and epsilon rises sharply; under the hybrid reading enforcement concentrates on gatekeeping administration instead of dissent sanction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_split, conceptual, 'Kernel-level reading indexicality: one of three incompatible membership criteria.').

omega_variable(
    epsilon_referent_reading_index,
    'Epsilon here is authored from the identity reading''s own lights over the fixed referent (the standing self-declaration arrangement); holders of the biology reading assess the same arrangement with far higher extraction — how much of the measured chi divergence across the sibling corpus is reading stance versus structural fact?',
    'Compare authored epsilon across the three sibling stories sharing this referent; divergence attributable to reading stance is expected corpus signal, not error, and separates cleanly once per-seat engine classifications are laid beside the authored values.',
    'Authored from the biology seat, the same arrangement would compute as heavily extractive capture of a protected category; the reading index keeps the referent fixed while the values legitimately diverge.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_reading_index, conceptual, 'Reading-indexed epsilon over a fixed referent across sibling stories.').

omega_variable(
    space_access_cost_concentration,
    'How concentrated are the dilution and conflict costs of widened space eligibility (prisons, refuges, changing rooms, sport), and do they concentrate enough on identifiable payer seats to shift per-seat classifications toward extraction-dominated verdicts?',
    'Incident, placement-policy, and eligibility-rule data disaggregated by setting; comparative studies of jurisdictions before and after the criterion changed.',
    'High concentration supports payer-seat extraction-dominated verdicts despite moderate story-level epsilon; diffuse costs keep the coordination frame dominant across seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(space_access_cost_concentration, empirical, 'Distribution and concentration of space-access conflict costs across payer seats.').

omega_variable(
    enforcement_ratchet_trajectory,
    'Does enforcement of the identity criterion ratchet — speech norms hardening, compliance mandates broadening, dissent sanctions formalizing — or stabilize now that codification has plateaued?',
    'Track policy diffusion, professional-body disciplinary actions, platform enforcement rates, and legislative amendments across the measurement interval.',
    'A continuing ratchet pushes suppression_requirement and epsilon upward and risks target-seat drift toward extraction-dominated verdicts; stabilization supports the tangled_rope reading as the settled equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_ratchet_trajectory, empirical, 'Whether enforcement machinery hardens or stabilizes post-codification.').

omega_variable(
    terminal_arrangement_vs_transitional_horizon,
    'Does the identity reading treat self-identification as the terminal classification arrangement, or as a transitional stage toward dissolution of gendered categories altogether — which would make the arrangement scaffold-shaped with an implicit rather than declared horizon?',
    'Doctrine analysis of leading identity-reading theorists for an articulated end-state beyond identity-based classification, plus institutional sponsorship of periodic-review or sunset-style clauses in self-ID statutes.',
    'If transitional, the arrangement''s justification is the crossing, not the steady state, and eventual obsolescence is designed-in; if terminal, mandatrophy questions reduce to ordinary drift monitoring of the sort the measurement series already tracks.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(terminal_arrangement_vs_transitional_horizon, preference, 'Terminal steady-state versus undeclared transitional horizon for the identity criterion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sex__tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(sex__tr_t0, observed).
narrative_ontology:measurement(sex__tr_t5, sex_gender_category__identity_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(sex__tr_t5, observed).
narrative_ontology:measurement(sex__tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.22).
narrative_ontology:measurement_basis(sex__tr_t10, observed).
narrative_ontology:measurement(sex__tr_t15, sex_gender_category__identity_reading, theater_ratio, 15, 0.27).
narrative_ontology:measurement_basis(sex__tr_t15, observed).
narrative_ontology:measurement(sex__tr_t20, sex_gender_category__identity_reading, theater_ratio, 20, 0.31).
narrative_ontology:measurement_basis(sex__tr_t20, observed).
narrative_ontology:measurement(sex__tr_t25, sex_gender_category__identity_reading, theater_ratio, 25, 0.35).
narrative_ontology:measurement_basis(sex__tr_t25, observed).
narrative_ontology:measurement(sex__tr_t30, sex_gender_category__identity_reading, theater_ratio, 30, 0.38).
narrative_ontology:measurement_basis(sex__tr_t30, observed).

% Extraction over time
narrative_ontology:measurement(sex__be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.16).
narrative_ontology:measurement_basis(sex__be_t0, observed).
narrative_ontology:measurement(sex__be_t5, sex_gender_category__identity_reading, base_extractiveness, 5, 0.19).
narrative_ontology:measurement_basis(sex__be_t5, observed).
narrative_ontology:measurement(sex__be_t10, sex_gender_category__identity_reading, base_extractiveness, 10, 0.23).
narrative_ontology:measurement_basis(sex__be_t10, observed).
narrative_ontology:measurement(sex__be_t15, sex_gender_category__identity_reading, base_extractiveness, 15, 0.26).
narrative_ontology:measurement_basis(sex__be_t15, observed).
narrative_ontology:measurement(sex__be_t20, sex_gender_category__identity_reading, base_extractiveness, 20, 0.29).
narrative_ontology:measurement_basis(sex__be_t20, observed).
narrative_ontology:measurement(sex__be_t25, sex_gender_category__identity_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement_basis(sex__be_t25, observed).
narrative_ontology:measurement(sex__be_t30, sex_gender_category__identity_reading, base_extractiveness, 30, 0.32).
narrative_ontology:measurement_basis(sex__be_t30, observed).

% Suppression requirement over time
narrative_ontology:measurement(sex__su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(sex__su_t0, observed).
narrative_ontology:measurement(sex__su_t5, sex_gender_category__identity_reading, suppression_requirement, 5, 0.36).
narrative_ontology:measurement_basis(sex__su_t5, observed).
narrative_ontology:measurement(sex__su_t10, sex_gender_category__identity_reading, suppression_requirement, 10, 0.42).
narrative_ontology:measurement_basis(sex__su_t10, observed).
narrative_ontology:measurement(sex__su_t15, sex_gender_category__identity_reading, suppression_requirement, 15, 0.47).
narrative_ontology:measurement_basis(sex__su_t15, observed).
narrative_ontology:measurement(sex__su_t20, sex_gender_category__identity_reading, suppression_requirement, 20, 0.51).
narrative_ontology:measurement_basis(sex__su_t20, observed).
narrative_ontology:measurement(sex__su_t25, sex_gender_category__identity_reading, suppression_requirement, 25, 0.54).
narrative_ontology:measurement_basis(sex__su_t25, observed).
narrative_ontology:measurement(sex__su_t30, sex_gender_category__identity_reading, suppression_requirement, 30, 0.55).
narrative_ontology:measurement_basis(sex__su_t30, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'who counts as a woman/man' decomposes, per the epsilon-invariance principle, into three structurally distinct classification constraints — identity, biology, and hybrid readings — each with a stable epsilon, distinct beneficiary/victim sets, and distinct enforcement economics. This file instantiates the identity reading; the siblings are linked here and link back. Lineage runs upstream: the biology reading historically grounded the category and still supplies the empirical anchor (anatomy is observable and stable), while the identity reading now contests and reshapes its legitimacy conditions downstream — each sibling story's affects_constraints array closes the triangle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(sex_gender_category__identity_reading, powerful, 0.62).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
