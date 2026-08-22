% ============================================================================
% CONSTRAINT STORY: sex_gender_category__identity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: sex_gender_category__identity_reading
 *   human_readable: Gender Category Membership by Self-Declared Identity (Identity Reading)
 *   domain: social ontology/legal classification/political
 *
 * SUMMARY:
 *   This file instantiates ONE reading of the sex_gender_category kernel: the
 *   identity reading, under which legal and institutional category membership
 *   ('woman', 'man') follows self-declared gender identity, with no medical
 *   or judicial gatekeeper. First enacted in Argentina (2012) and since
 *   adopted in varying forms in Denmark, Ireland, Malta, Norway, Germany, and
 *   elsewhere, the rule produces the expected structural delta: trans women
 *   are included in the 'woman' category, the victim set of misogyny expands
 *   to include trans women (now protected as category members), cis women
 *   lose the exclusive claim to sex-based protections, boundary enforcement
 *   is administratively cheap, and conflict concentrates on space access. The
 *   claim and the metrics are independent authored facts: the claimed type is
 *   tangled_rope because the rule possesses both a genuine coordination
 *   function (a uniform gatekeeper-free criterion replacing per-case medical
 *   determination) and an asymmetric transfer (exclusivity dilution and
 *   enforcement burdens falling on identifiable groups), and the metrics
 *   describe the rule's actual operation as observed over the adoption era.
 *   The epsilon referent is the self-identification arrangement itself,
 *   assessed by this reading's own lights; the sibling readings are separate
 *   constraint files linked through the network, not hedges folded into this
 *   one.
 *
 * KEY AGENTS:
 *   - trans_women: primary beneficiary (moderate/identity_locked) - gain category membership, protection coverage, and space access; absorb boundary-conflict exposure as a secondary cost
 *   - trans_men: parallel beneficiary (moderate/identity_locked) - recognition rides on the same declaration rule at lower visibility
 *   - gender_identity_advocacy_organizations: secondary beneficiary (organized/mobile) - framework codified into statute, institutional footprint expands
 *   - cis_women_in_single_sex_settings: primary target (powerless/constrained) - bear dilution of single-sex guarantees in refuges, prisons, wards, and changing rooms
 *   - female_competitive_athletes: target where sport opens (moderate/constrained) - bear eligibility consequences unevenly across levels and federations
 *   - conscientious_objecting_service_providers: target (moderate/identity_locked) - bear enforcement costs of refusal, with mission identity fused to single-sex provision
 *   - national_legislatures_and_courts: agenda setter (institutional/arbitrage) - enact, interpret, and amend the criterion
 *   - incarcerated_women: excluded voice (powerless/trapped) - placement decided over them through litigation and guidance
 *   - statistical_data_users: excluded voice (moderate/mobile) - data-integrity objections arrived outside the consultation rooms
 *   - equality_regulators: analytical observer (institutional/analytical) - synthesize evidence and issue implementation guidance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sex_gender_category__identity_reading, 0.35).
domain_priors:suppression_score(sex_gender_category__identity_reading, 0.55).
domain_priors:theater_ratio(sex_gender_category__identity_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(sex_gender_category__identity_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sex_gender_category__identity_reading, tangled_rope).
narrative_ontology:human_readable(sex_gender_category__identity_reading, "Gender Category Membership by Self-Declared Identity (Identity Reading)").
narrative_ontology:topic_domain(sex_gender_category__identity_reading, "social ontology/legal classification/political").

domain_priors:requires_active_enforcement(sex_gender_category__identity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sex_gender_category__identity_reading, '17737a20-ecd7-4165-b9ca-d79cad7b0b9f').
narrative_ontology:cs_kernel_codification('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', formalized).
narrative_ontology:cs_authority_grounding('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', self_enforcing).
narrative_ontology:cs_reading_relation('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', sex_gender_category__biology_reading, forecloses).
narrative_ontology:cs_reading_relation('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', sex_gender_category__hybrid_reading, forecloses).
narrative_ontology:cs_axiom('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', foundational, self_declared_identity_constitutes_membership).
narrative_ontology:cs_axiom_status(self_declared_identity_constitutes_membership, holdable).
narrative_ontology:cs_axiom_grounding('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', self_declared_identity_constitutes_membership, deontological).
narrative_ontology:cs_axiom('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', secondary, recognition_requires_no_medical_diagnosis).
narrative_ontology:cs_axiom_status(recognition_requires_no_medical_diagnosis, holdable).
narrative_ontology:cs_axiom_grounding('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', recognition_requires_no_medical_diagnosis, instrumental).
narrative_ontology:cs_reference_frame('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', self_identification_baseline).
narrative_ontology:cs_drift_state('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', contemporary_post_adoption_backlash_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('17737a20-ecd7-4165-b9ca-d79cad7b0b9f', '').
narrative_ontology:cs_kernel_id(sex_gender_category__identity_reading, sex_gender_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_women).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, trans_men).
narrative_ontology:constraint_beneficiary(sex_gender_category__identity_reading, gender_identity_advocacy_organizations).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, cis_women_in_single_sex_settings).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, female_competitive_athletes).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, conscientious_objecting_service_providers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sex_gender_category__identity_reading, trans_women).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live under a classification rule that treats their self-declared identity as decisive for legal documents, anti-discrimination protection, and access to women's facilities and services. What flows to them: recognition without psychiatric diagnosis or surgery, protection coverage as category members (including protection from misogyny directed at them), and access to spaces previously closed. What also flows: heightened public scrutiny and conflict exposure at boundaries such as bathrooms, shelters, and sport, where their presence is contested. Their stake in how the category is defined is constitutive of their legal existence; stepping outside the classification question is not available to them.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_women, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(sex_gender_category__identity_reading, trans_women, payer).

% Gain parallel recognition: self-declared identity places them in the 'man' category for documents and protections. Boundary conflicts around men's spaces are lower-stakes and lower-visibility than around women's spaces, so their day-to-day friction is smaller, but their legal position rides on the same declaration rule and would revert with it.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, trans_men, beneficiary,
    moderate, biographical, identity_locked, national).

% Campaign for and defend the declaration rule; where it passes, their framework is written into statute and they supply consultees, drafters, and litigation support. Each adoption expands their institutional footprint, funding base, and standing inside equality bodies. If one jurisdiction closes, they can redirect effort to others.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, gender_identity_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).

% Use or depend on single-sex provisions such as refuges, prison placement, hospital wards, and changing rooms, whose guarantee previously rested on the category excluding male-bodied entrants. Under the declaration rule the guarantee becomes discretionary: providers admit on identity, and recourse for objecting users runs through complaints processes they rarely control. Opting out of a setting that admits on self-declaration is limited by need; a woman fleeing violence cannot shop among refuges on principle.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, cis_women_in_single_sex_settings, payer,
    powerless, immediate, constrained, national).

% Compete in categories whose eligibility criteria some jurisdictions and grassroots bodies open to identity-based entry. Where that happens they bear training-and-selection consequences: roster spots, prize structures, and records set under changed entry rules. Elite international federations have largely retained sex-based or hormone-based criteria, so exposure is uneven across levels and sports, and individually they hold little leverage over the bodies that set the rules.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, female_competitive_athletes, payer,
    moderate, biographical, constrained, global).

% Rape crisis centers, women's refuges, and some faith-based charities whose service model is built on biologically female-only provision. Refusing identity-based admission exposes them to funding conditionality, commissioning exclusion, and litigation; complying dissolves the service model their staff and donors signed up for. Their organizational mission is fused with single-sex provision, so closing or converting means abandoning the population they exist to serve.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, conscientious_objecting_service_providers, payer,
    moderate, generational, identity_locked, national).

% Enact, interpret, and amend the declaration rule. Statutes set the criterion; courts police refusals and define exceptions for prisons, sport, and data collection. They can revise the rule by ordinary legislation and absorb the political heat of doing so in either direction.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, national_legislatures_and_courts, agenda_setter,
    institutional, generational, arbitrage, national).

% Housed by authorities applying the classification rule to placement decisions. They object to mixed housing units and to policy set through litigation and administrative guidance rather than consultation; their voice in the decisions governing their daily environment is minimal, and they cannot leave for the duration of a sentence.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, incarcerated_women, excluded,
    powerless, biographical, trapped, national).

% Demographers, epidemiologists, and actuaries who rely on stable sex-disaggregated series. Identity-based re-registration migrates individuals between series mid-stream, and they object that trend lines and denominator bases degrade. They were consulted late or not at all in most legislative processes; their remedy is methodological workaround rather than voice.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, statistical_data_users, excluded,
    moderate, generational, mobile, global).

% Equality and human-rights bodies that issue statutory guidance on how the declaration rule interacts with single-sex exceptions, collect evidence from all sides, and advise legislatures. They hold no vote on the rule, but their guidance shapes how providers implement it.
narrative_ontology:constraint_stakeholder(sex_gender_category__identity_reading, equality_regulators, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sex_gender_category__identity_reading, trans_women).
narrative_ontology:fixing_cost_class(sex_gender_category__identity_reading, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform, gatekeeper-free criterion for legal gender classification: registries, anti-discrimination bodies, and service allocators apply one rule (self-declaration) instead of per-case medical or judicial determination. It coordinates document issuance, protection coverage, and record consistency across institutions without requiring diagnosis, surgery, or a court order.
% TRANSFER_FUNCTION: Moves category membership, and the protections, space access, and documentation rights attached to it, to people whose identity differs from birth registration. Correspondingly it moves the exclusivity of category-based provisions (single-sex services, sports categories, sex-disaggregated statistics) away from natal members, and moves compliance and enforcement burdens onto institutions that decline the criterion.
% ABSENT_VOICES: Incarcerated women: space-placement decisions are made over them by prison authorities and through litigation, with little direct voice in consultations. Statistical data users raised data-integrity objections late and outside the main legislative hearings. Parents of school-age children, in jurisdictions applying social-transition policies without parental notification, were largely outside the consultation rooms.
% DISAPPEARANCE_RATIONALE: Overnight removal would revert legal gender recognition to medicalized or judicial gatekeeping, or to nothing: existing corrected documents would face re-correction, anti-discrimination coverage would contract, institutions would rebuild gatekeeping apparatuses, and space-allocation practices across refuges, prisons, and services would reorganize around whatever successor criterion each jurisdiction adopts.
% FOUNDING_PROBLEM: Legal gender recognition historically required psychiatric diagnosis, sterilization or irreversible surgery, divorce, and court proceedings. The arrangement was built to end that gatekeeping, which pathologized trans people and exposed them to invasive, humiliating requirements to obtain basic document consistency.
% FOUNDING_PROBLEM_CORROBORATION: Outside the beneficiary set: the European Court of Human Rights (Goodwin v UK, 2002) attested the distress and legal incoherence of non-recognition before any self-ID statute existed; the WHO ICD-11 revision (2019) removed gender incongruence from mental-disorder chapters, corroborating the depathologizing premise; national medical associations in several adopting states submitted evidence that gatekeeping requirements served no clinical purpose. On the status question, no independent body attests that the founding problem is dead; the dead-problem assertion originates with critics of the arrangement, while the live-problem assertion is corroborated by the continued existence of gatekeeping regimes in most of the world's jurisdictions.
narrative_ontology:disappearance_verdict(sex_gender_category__identity_reading, world_rearranges).
narrative_ontology:founding_problem_status(sex_gender_category__identity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sex_gender_category__identity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sex_gender_category__identity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sex_gender_category__identity_reading, 0.35, 'stealth/ox-alpha', 'none', direct).

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
 *   Extractiveness is authored at 0.35: the rule's dominant operation is inclusionary recognition (document correction, protection coverage, gatekeeping removal), but a real transfer runs through the same structure - cis women's exclusive claim to sex-based provisions is diluted, and institutions that refuse the criterion bear enforcement costs. Even by this reading's own lights, those costs are acknowledged side-effects to be managed, which caps epsilon well below snare territory while keeping it clearly above zero. Suppression is 0.55: alternatives (the biology and hybrid readings) are not logically erased - they remain live in many jurisdictions and courts - but institutional expression of them is penalized through legal liability for refusers, funding conditionality, and employment consequences; roughly 60% of the measured suppression is structural and 40% internalized (reputational fear and self-censorship), a split carried by the suppression_structural_vs_internalized omega. Theater_ratio is low (0.15): the core function is real and performed daily in registries and services; performative activity is confined to institutional signaling. Accessibility_collapse is 0.35: understanding the rule does not collapse the alternatives, which persist as live legislation, litigation, and rival statutes across jurisdictions. Resistance is high (0.70): sustained parliamentary fights, a blocked Scottish gender-recognition bill, sports-federation reversals, systematic-review interventions, and active litigation. The measurement series run on one shared time grid (T=0..12, mapped to 2012-2024, Argentina's Gender Identity Law to Germany's Self-Determination Act) with all three metrics authored at every point; trajectories are monotonic - public attention oscillates with incident cycles, but the structural quantities drift steadily, with enforcement machinery hardening as dissent organizes. FNL coupling caution: the identity_coordination type grants a complexity offset, but the extraction here concentrates on powerless agents (incarcerated women, refuge users) at national scope; the offset accommodates genuine boundary-maintenance complexity and does not excuse that concentration.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and should. From the trans_women seat the arrangement is recognition: a rule that removes psychiatric gatekeeping and extends protection coverage reads as coordination they built their legal existence on. From the cis_women_in_single_sex_settings seat the same rule operates as an unconsented dilution of a protective guarantee, with recourse channels they do not control. From the agenda_setter seat it is an administrable criterion that trades a cheap declaration rule for a hot political perimeter. From the conscientious provider seat it is a forced choice between litigation and mission dissolution. The engine derives these divergent classifications from the structural data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation. trans_women and trans_men sit near the beneficiary pole (low d), with a slight upward adjustment for trans_women reflecting the conflict-exposure costs they absorb as the boundary fight intensifies - hence the secondary payer role. gender_identity_advocacy_organizations derive low d as beneficiaries whose framework is vindicated and whose footprint grows. cis_women_in_single_sex_settings derive high d: they bear the exclusivity transfer with constrained exit, and trapped-adjacent conditions (needing a refuge, serving a sentence) amplify effective extraction. female_competitive_athletes derive high d where the rule reaches their category, moderated by the sports carve-out documented in the sports_domain_separability omega. conscientious_objecting_service_providers derive high d with identity_locked exit amplifying their exposure. national_legislatures_and_courts sit mid-range: they administer the rule and absorb its political costs. The excluded seats - incarcerated_women above all - experience near-full-target conditions on the decisions made over them, without voice; statistical_data_users bear diffuse epistemic costs. Suppression is authored as a raw structural property and is not scaled by power or scope; only extractiveness is scaled, by directionality and spatial scope in the engine's computation.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents symmetric mislabeling. Read without the coordination/extraction split, critics collapse the whole rule into pure extraction (a boundary violation dressed as recognition) and proponents collapse it into pure coordination (recognition with no costs). The tangled_rope verdict holds both truths: the coordination function is genuine - a single gatekeeper-free criterion replaced per-case medical and judicial determination, solving a real collective-action problem - and the same structure transfers exclusivity from natal members to self-identifying entrants and pushes enforcement costs onto dissenting providers, which is why it requires active enforcement to hold. On genealogy: the founding problem (medicalized gatekeeping) is substantially solved in adopting jurisdictions, but the parties dispute whether the solution is complete or has spawned successor problems; the founding_problem_status is therefore contested, and the mismatch consumer watches for the zombie pattern - a rule persisting after its founding problem is dead - which would surface as status=dead combined with verdict=world_rearranges. Fixing_cost is authored as cheap: the arrangement is young, statutorily based, and reversible by ordinary legislation, so its persistence reflects ongoing political contest rather than fix-cost lock-in; gain_flow is authored as trans_women because the transferred good - category access and its attached protections - demonstrably accrues to that seat.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the sex_gender_category kernel (reading: identity_reading). What would the sibling readings change structurally, and where exactly is the disagreement located?',
    'Compare against the sibling files sex_gender_category__biology_reading and sex_gender_category__hybrid_reading: the biology reading restores cis women''s exclusive claim and removes trans women from the category; the hybrid reading conditions membership on medical transition. The disagreement is located in the determinant of membership: sufficiency of self-declaration versus immutable biology versus biology-plus-transition.',
    'Cross-reading comparisons that treat the three files as one constraint will average incompatible victim sets and epsilon values; each reading must be classified on its own structural data.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this file instantiates the identity reading of a three-reading kernel; siblings are separate constraints, not hedges inside this one.').

omega_variable(
    epsilon_referent_fixation,
    'Is the epsilon value indexed to the correct referent: the self-identification arrangement itself, assessed by this reading''s own lights, rather than to the biology-based arrangement this reading opposes?',
    'Audit that the beneficiary/victim declarations and metric scores describe the self-ID regime as operated (who gains membership, who loses exclusivity, what enforcement costs arise), not the counterfactual regime the reading rejects.',
    'A referent slip would import the biology reading''s extraction profile into this file and produce a spurious cross-reading delta; the correct referent keeps the three files comparable at the kernel level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(epsilon_referent_fixation, conceptual, 'Epsilon referent discipline for kernel readings: the standing arrangement under contest is the self-ID arrangement, never the reading''s endorsed alternative.').

omega_variable(
    exclusivity_loss_materiality,
    'How materially do cis women lose safety, privacy, or protection through identity-based admission in specific settings (prisons, refuges, hospital wards, changing rooms), as distinct from how the conflict over those settings is publicly dramatized?',
    'Setting-level outcome data: incident rates, complaint volumes, refuge-placement outcomes, and prison-placement reviews in adopting jurisdictions, compared against matched non-adopting jurisdictions.',
    'Material, concentrated losses would raise effective extraction on the cis-women seat and push the computed type toward the snare end of the hybrid range; negligible losses would support a rope-leaning reading with the conflict treated as symbolic rather than structural.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusivity_loss_materiality, empirical, 'Whether the exclusivity dilution borne by cis women is a material transfer or a dramatized boundary dispute.').

omega_variable(
    suppression_structural_vs_internalized,
    'Of the measured suppression of dissent from the declaration rule, how much is structural (legal liability for refusers, funding conditionality, employment consequences) and how much is internalized (reputational fear and self-censorship that would persist if enforcement relaxed)?',
    'Post-relaxation trajectory: track dissent expression in jurisdictions where enforcement has weakened or courts have narrowed the rule; if open dissent resurges quickly, the internalized share is small; if it does not, internalized suppression dominates.',
    'If internalized suppression dominates, the constraint''s effective suppression exceeds the structural measure and would outlive statutory change; if structural mechanisms dominate, repealing or narrowing the rule releases the pressure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized split of the suppression holding alternative readings down.').

omega_variable(
    bad_faith_invocation_rate,
    'What is the actual rate of bad-faith self-identification invoked to gain access to women''s spaces or categories, as opposed to the rate asserted in public argument by critics and dismissed by proponents?',
    'Adjudicated-case audits in adopting jurisdictions: documented instances of access sought under the declaration rule in deliberate bad faith, separated from contested-good-faith cases and from fabricated anecdotes.',
    'A near-zero adjudicated rate supports treating the rule''s extraction as limited to exclusivity dilution; a material adjudicated rate would constitute a distinct extraction channel and raise epsilon on every payer seat.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(bad_faith_invocation_rate, empirical, 'Base rate of exploitative invocation of the self-declaration mechanism.').

omega_variable(
    sports_domain_separability,
    'Is competitive-sport eligibility structurally separable from the general classification rule, given that most frameworks carve sport out with sex-based or hormone-based criteria?',
    'Track whether jurisdictions and institutions that adopt the declaration rule consistently preserve separate sport criteria; if the carve-out holds universally, the athlete seat''s costs route through a different constraint.',
    'If separable, the female_competitive_athletes seat''s extraction belongs to the sports-eligibility constraint, not this one, and this file''s victim set narrows; if the carve-out erodes, this constraint inherits the athlete seat''s full extraction weight.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sports_domain_separability, conceptual, 'Whether sport is a genuine exception domain or a delayed application of the same classification rule.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sex_gender_category__identity_reading, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sgc_identity_reading_tr_t0, sex_gender_category__identity_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(sgc_identity_reading_tr_t2, sex_gender_category__identity_reading, theater_ratio, 2, 0.09).
narrative_ontology:measurement(sgc_identity_reading_tr_t4, sex_gender_category__identity_reading, theater_ratio, 4, 0.1).
narrative_ontology:measurement(sgc_identity_reading_tr_t6, sex_gender_category__identity_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(sgc_identity_reading_tr_t8, sex_gender_category__identity_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(sgc_identity_reading_tr_t10, sex_gender_category__identity_reading, theater_ratio, 10, 0.14).
narrative_ontology:measurement(sgc_identity_reading_tr_t12, sex_gender_category__identity_reading, theater_ratio, 12, 0.15).

% Extraction over time
narrative_ontology:measurement(sgc_identity_reading_be_t0, sex_gender_category__identity_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(sgc_identity_reading_be_t2, sex_gender_category__identity_reading, base_extractiveness, 2, 0.25).
narrative_ontology:measurement(sgc_identity_reading_be_t4, sex_gender_category__identity_reading, base_extractiveness, 4, 0.28).
narrative_ontology:measurement(sgc_identity_reading_be_t6, sex_gender_category__identity_reading, base_extractiveness, 6, 0.3).
narrative_ontology:measurement(sgc_identity_reading_be_t8, sex_gender_category__identity_reading, base_extractiveness, 8, 0.32).
narrative_ontology:measurement(sgc_identity_reading_be_t10, sex_gender_category__identity_reading, base_extractiveness, 10, 0.34).
narrative_ontology:measurement(sgc_identity_reading_be_t12, sex_gender_category__identity_reading, base_extractiveness, 12, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(sgc_identity_reading_su_t0, sex_gender_category__identity_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(sgc_identity_reading_su_t2, sex_gender_category__identity_reading, suppression_requirement, 2, 0.41).
narrative_ontology:measurement(sgc_identity_reading_su_t4, sex_gender_category__identity_reading, suppression_requirement, 4, 0.46).
narrative_ontology:measurement(sgc_identity_reading_su_t6, sex_gender_category__identity_reading, suppression_requirement, 6, 0.49).
narrative_ontology:measurement(sgc_identity_reading_su_t8, sex_gender_category__identity_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(sgc_identity_reading_su_t10, sex_gender_category__identity_reading, suppression_requirement, 10, 0.54).
narrative_ontology:measurement(sgc_identity_reading_su_t12, sex_gender_category__identity_reading, suppression_requirement, 12, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sex_gender_category__identity_reading, identity_coordination).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__biology_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sex_gender_category__hybrid_reading).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, single_sex_service_provision_rules).
narrative_ontology:affects_constraint(sex_gender_category__identity_reading, sports_category_eligibility_rules).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what determines gender category membership' decomposes into three structurally distinct classification rules - biology_reading, hybrid_reading, identity_reading - each with its own epsilon, beneficiary/victim structure, and classification. The decomposition follows the epsilon-invariance principle: measuring membership by self-declaration versus by biology yields different victim sets and different extraction profiles, so they are different constraints, not one constraint with a measurement parameter. Upstream/downstream structure: the biology reading is the historical baseline from which the hybrid reading emerged as a medicalized compromise; the identity reading removes the gatekeeping layer entirely and creates structural downstream pressure on both siblings by shifting legitimacy conditions (statutes citing self-determination, litigation framing biology-based exclusion as discrimination) without logically resolving the dispute across frameworks. Within any single framework this reading forecloses both siblings as determinants of the same category; across frameworks all three coexist as live positions. This file links to both siblings and to the two domain constraints (single-sex provision, sports eligibility) that inherit its boundary decisions.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
