% ============================================================================
% CONSTRAINT STORY: woman_category__sex_biology_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_woman_category__sex_biology_reading, []).

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
 *   constraint_id: woman_category__sex_biology_reading
 *   human_readable: Sex-Biological Definition of the Woman Category (Typical-Case Reading)
 *   domain: political philosophy/law/social policy/bioethics
 *
 * SUMMARY:
 *   The sex-biology reading fixes category membership for 'woman' by
 *   chromosomal, anatomical, and reproductive biology — the adult human
 *   female with the typical case's XX karyotype and female reproductive
 *   anatomy — and operationalizes that rule in birth registration, statutory
 *   definition, single-sex service allocation, elite-sport eligibility, and
 *   sex-stratified data collection. The rule solves genuine coordination
 *   problems while imposing asymmetric costs on those its verification
 *   machinery touches: transgender women excluded from sex-segregated
 *   protections, intersex people assigned ambiguously and normalized without
 *   consent, and athletes with differences of sex development screened out of
 *   the careers their bodies built. This file instantiates ONE reading of the
 *   contested woman_category kernel; the sibling readings are separate
 *   constraints with their own victim sets and epsilon values, linked via
 *   network.affects_constraints. Claim and metrics are independent: the
 *   constraint is CLAIMED as tangled_rope — genuine coordination carrying
 *   asymmetric extraction — while the metrics describe substantially
 *   extractive, actively enforced operation that intensified over the
 *   interval. KEY AGENTS (by structural relationship): -
 *   sports_governing_bodies: agenda-setter (institutional/arbitrage) — sets
 *   female-category eligibility, commissions the supporting science, operates
 *   testing and review - legislative_judicial_authorities: agenda-setter
 *   (institutional/constrained) — enacts statutory sex definitions,
 *   adjudicates challenges - female_athletes_in_protected_categories: primary
 *   beneficiary (organized/constrained) — competes inside the protected
 *   category - women_using_single_sex_services: beneficiary
 *   (moderate/constrained) — relies on biologically-defined allocation of
 *   shelters, refuges, placements - medical_research_establishment:
 *   beneficiary (institutional/mobile) — consumes the stable sex variable for
 *   clinical and research stratification -
 *   transgender_women_excluded_from_protections: primary target
 *   (powerless/trapped) — classified outside the category they live as -
 *   intersex_people: target (powerless/trapped) — assigned ambiguously,
 *   normalized without consent, verified when the rule bites -
 *   dsd_elite_athletes: target (moderate/trapped) — screened out of their
 *   careers unless they medically alter - transgender_men: target
 *   (powerless/trapped) — retained in the female category against lived
 *   identity - gender_nonconforming_cisgender_women: dual beneficiary/target
 *   (moderate/constrained) — holds the protections, absorbs the informal
 *   verification costs - intersex_led_advocacy_groups: excluded voice
 *   (organized/constrained) — absent from the design of the practices
 *   affecting them - bioethical_analytical_observers: analytical observer —
 *   sees the full structure across readings
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(woman_category__sex_biology_reading, 0.66).
domain_priors:suppression_score(woman_category__sex_biology_reading, 0.67).
domain_priors:theater_ratio(woman_category__sex_biology_reading, 0.46).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, extractiveness, 0.66).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, theater_ratio, 0.46).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(woman_category__sex_biology_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(woman_category__sex_biology_reading, tangled_rope).
narrative_ontology:human_readable(woman_category__sex_biology_reading, "Sex-Biological Definition of the Woman Category (Typical-Case Reading)").
narrative_ontology:topic_domain(woman_category__sex_biology_reading, "political philosophy/law/social policy/bioethics").

domain_priors:requires_active_enforcement(woman_category__sex_biology_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(woman_category__sex_biology_reading, '0716d27f-b6da-43ef-8cb5-d31a017b057a').
narrative_ontology:cs_kernel_codification('0716d27f-b6da-43ef-8cb5-d31a017b057a', formalized).
narrative_ontology:cs_authority_grounding('0716d27f-b6da-43ef-8cb5-d31a017b057a', lineage).
narrative_ontology:cs_interpretation_layer_present('0716d27f-b6da-43ef-8cb5-d31a017b057a').
narrative_ontology:cs_reading_relation('0716d27f-b6da-43ef-8cb5-d31a017b057a', woman_category__gender_identity_reading, forecloses).
narrative_ontology:cs_reading_relation('0716d27f-b6da-43ef-8cb5-d31a017b057a', woman_category__intersex_accommodation_reading, influences).
narrative_ontology:cs_axiom('0716d27f-b6da-43ef-8cb5-d31a017b057a', foundational, category_membership_tracks_biological_sex).
narrative_ontology:cs_axiom_status(category_membership_tracks_biological_sex, holdable).
narrative_ontology:cs_axiom_grounding('0716d27f-b6da-43ef-8cb5-d31a017b057a', category_membership_tracks_biological_sex, instrumental).
narrative_ontology:cs_axiom('0716d27f-b6da-43ef-8cb5-d31a017b057a', foundational, sex_is_binary_objective_kind).
narrative_ontology:cs_axiom_status(sex_is_binary_objective_kind, holdable).
narrative_ontology:cs_axiom_grounding('0716d27f-b6da-43ef-8cb5-d31a017b057a', sex_is_binary_objective_kind, empirically_contingent).
narrative_ontology:cs_reference_frame('0716d27f-b6da-43ef-8cb5-d31a017b057a', binary_biological_sex_classification).
narrative_ontology:cs_drift_state('0716d27f-b6da-43ef-8cb5-d31a017b057a', contemporary_post_gender_identity_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0716d27f-b6da-43ef-8cb5-d31a017b057a', '').
narrative_ontology:cs_kernel_id(woman_category__sex_biology_reading, woman_category).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, female_athletes_in_protected_categories).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, women_using_single_sex_services).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, medical_research_establishment).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_women_excluded_from_protections).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, intersex_people).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, dsd_elite_athletes).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, transgender_men).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(woman_category__sex_biology_reading, gender_nonconforming_cisgender_women).
narrative_ontology:constraint_victim(woman_category__sex_biology_reading, gender_nonconforming_cisgender_women).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sexual_dimorphism_in_human_physiology).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, sex_stratified_clinical_efficacy).
narrative_ontology:constraint_vindicates(woman_category__sex_biology_reading, fair_competition_requires_sex_categories).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Set and revise eligibility rules for the female category in their sports, commission the physiological frameworks used to justify them, and operate the testing and review procedures that decide who may compete. International federations can relocate headquarters and rewrite rules across jurisdictions; their authority over the category grows each time a boundary case reaches them.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, sports_governing_bodies, agenda_setter,
    institutional, generational, arbitrage, global).

% Enact statutory definitions of sex and woman for documentation, services, and equality law, and adjudicate challenges to those definitions. Bound by constitutional structure and precedent within their jurisdictions; once the question is asked they cannot decline it, only answer it differently.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, legislative_judicial_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Compete for places in female categories whose eligibility rules keep out athletes with male-typical physiology. Medal prospects and livelihoods depend on the category holding; there is no exit from the eligibility regime short of leaving elite sport.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, female_athletes_in_protected_categories, beneficiary,
    organized, biographical, constrained, global).

% Use shelters, refuges, changing rooms, and custodial placements allocated on the basis of sex as biologically defined. They rely on the category for safety and privacy guarantees; the alternative is accepting mixed provision where it exists.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, women_using_single_sex_services, beneficiary,
    moderate, biographical, constrained, national).

% Stratifies diagnosis, dosing, screening programs, and research cohorts by recorded sex. Gains a stable variable that organizes reproducible clinical knowledge; bears little of the definitional dispute's cost and can re-stratify datasets if definitions change.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, medical_research_establishment, beneficiary,
    institutional, generational, mobile, global).

% Live administratively and socially as women but are classified outside the female category for services, sport, and data collection under this definition. Documentation histories and sporting eligibility follow them across borders; opting out of the classification applied to them is not available.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_women_excluded_from_protections, payer,
    powerless, biographical, trapped, national).

% Born with variations in chromosomes, gonads, or anatomy that do not match the typical case. Many underwent nonconsensual infancy surgeries intended to fit them to one box; as adults they face verification demands wherever the category is enforced strictly. The definition assigns them ambiguously, and the ambiguity is resolved by others.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_people, payer,
    powerless, generational, trapped, global).

% Elite competitors with male-typical chromosomes or testosterone arising from differences of sex development. Eligibility rules require them to medically alter their bodies or leave the female category; their careers exist only inside the regime that screens them, and the screening follows them to every competition on earth.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, dsd_elite_athletes, payer,
    moderate, biographical, trapped, global).

% Registered female at birth and living as men, but retained in the female category for services, custodial placement, and data under this definition. Placement follows their recorded sex rather than their lives; escaping the classification generally requires changing the definition itself, not their circumstances.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, transgender_men, payer,
    powerless, biographical, trapped, national).

% Women whose appearance or manner draws informal challenges to their presence in female spaces. They hold the category's protections but also absorb its verification costs — being questioned, delayed, or removed when others police the boundary in hallways, restrooms, and waiting rooms.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, gender_nonconforming_cisgender_women, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(woman_category__sex_biology_reading, gender_nonconforming_cisgender_women, payer).

% Organizations led by people with variations of sex characteristics, campaigning against nonconsensual intervention and for self-determined classification. They were largely absent from the medical and administrative decisions that built the current verification and normalization practices, and enter mainly through litigation and consultation after rules are set.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, intersex_led_advocacy_groups, excluded,
    organized, generational, constrained, global).

% Philosophers, bioethicists, and legal theorists who analyze the category dispute across all readings. They take testimony from every seat, publish analyses of the criteria and their costs, and hold no stake in which definition prevails.
narrative_ontology:constraint_stakeholder(woman_category__sex_biology_reading, bioethical_analytical_observers, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(woman_category__sex_biology_reading, diffuse).
narrative_ontology:fixing_cost_class(woman_category__sex_biology_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves real coordination problems: fair competition categories given physiological performance distributions; sex-stratified clinical care and research (dosing, disease presentation, screening); allocation of protective single-sex spaces and services designed around patterns correlated with female biology; and consistent vital-statistics and crime-data collection.
% TRANSFER_FUNCTION: Moves categorical membership and everything keyed to it — access to female-category competition, single-sex services and placements, inclusion in sex-stratified protections and datasets — to persons with typical female biology, and moves the costs of boundary maintenance (testing, verification, normalization pressure, exclusion) onto transgender women, intersex people, and athletes with differences of sex development.
% ABSENT_VOICES: Intersex-led organizations were absent when medical normalization protocols and early verification regimes were designed; transgender people were largely absent from the legislative chambers where recent statutory definitions were drafted; intersex infants had no voice at all in the surgeries performed on them. Their objections enter late, through litigation and consultation, after the rules are set.
% DISAPPEARANCE_RATIONALE: Sport eligibility, single-sex service allocation, medical stratification, legal documentation, and crime-data collection all key to this definition; removing it overnight forces simultaneous re-specification of every dependent rule. The scale of that rearrangement is precisely why the sibling readings compete so fiercely over the replacement.
% FOUNDING_PROBLEM: Stable, verifiable classification of persons for documentation, medicine, sport, and protective services in an era when sex appeared binary and self-evident — concretely, protecting female-class persons from male-pattern violence and preserving fair female competition given male-puberty physiological advantages.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties as a PROBLEM: sports physiologists attest the performance-distribution issue, criminologists and victimization surveys attest sex-patterned violence, clinical epidemiology attests medical sex differences. But no source outside the beneficiary set attests that the typical-case biological rule is the required SOLUTION — intersex-led organizations and trans advocacy groups explicitly dispute that step, and this story states plainly that the rule itself rests its case on the benefiting parties' own institutions.
narrative_ontology:disappearance_verdict(woman_category__sex_biology_reading, world_rearranges).
narrative_ontology:founding_problem_status(woman_category__sex_biology_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(woman_category__sex_biology_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(woman_category__sex_biology_reading, 'none', 1).
narrative_ontology:epsilon_provenance(woman_category__sex_biology_reading, 0.66, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(woman_category__sex_biology_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(woman_category__sex_biology_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(woman_category__sex_biology_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness ends at 0.66: the definition's costs are decoupled from any service rendered to those paying them — a DSD athlete receives nothing from the regulation that removes her; an intersex infant received nothing from the surgery performed on her. The series dips mid-interval (mass chromosomal testing abandoned circa t24) then climbs as targeted DSD regulations and statutory codification concentrate costs on narrower classes. Suppression (0.67 scalar; series U-shaped) traces enforcement capacity: mandatory mass sex testing at t0, decay to case-by-case handling by t24, then deliberate rebuilding through hyperandrogenism rules, testosterone thresholds, and finally statutory entrenchment — a build-decay-rebuild cycle driven by verification technology and political salience, and the oscillation is partly an extraction mechanism itself: each rebuild targets a smaller population with harder tools while the surrounding politics grow more polarized. Theater ratio rises monotonically (0.15 to 0.46) as symbolic definitional politics outpaces the practical caseload — most legislative activity now concerns boundary cases affecting tiny numbers, sustained for constituency-signaling value. Accessibility_collapse 0.52: alternatives do not vanish — sibling readings operate in other jurisdictions — but within an adopting jurisdiction the legal alternative space collapses almost entirely. Resistance 0.72: sustained litigation (the Semenya line through CAS and the European Court of Human Rights), intersex-led campaigns against infant surgery, athletic-union organizing, and academic contestation. Suppression here is overwhelmingly structural (legal definitions, testing mandates, custody rules) with a minor internalized residue (historical athlete acquiescence to testing framed as routine procedure); it is authored as a raw structural property and is not scaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, which is why the global-scope sport domain carries the highest effective extraction. All three temporal series run on one shared eight-point grid. Receipt surface: gain_flow is authored as 'diffuse' after checking every seat — the institutions accrue administrative authority, the female class accrues protection and category benefits spread across millions of holders, and no single named seat captures the extraction itself. fixing_cost is 'prohibitive': redefinition requires rewriting statutes, renegotiating international sport frameworks, and absorbing constituency backlash and litigation exposure that dwarf, for the seated agenda-setters, the benefit of relieving the excluded minority classes.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the agenda-setter seats compute differently from the same structure. From the sports_governing_bodies and legislative_judicial_authorities seats the arrangement is legitimate administration of a scientific classification; from the trapped payer seats (transgender women, intersex people, DSD athletes, transgender men) the same structure operates as enforced exclusion with no exit; from the beneficiary seats it is protection and epistemic stability. The engine computes this per-seat divergence from the structural data — the authored claim does not adjudicate it. Coalition dynamics matter here: the payer classes are dispersed and individually powerless, but trans-intersex advocacy alliances, athlete unions, and litigating athletes have begun converting dispersed harm into coordinated resistance, which is the main upward pressure on the resistance metric and the main threat to the arrangement's persistence.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (female_athletes_in_protected_categories, women_using_single_sex_services, medical_research_establishment) derive low directionality — the constraint subsidizes these seats, damping effective extraction toward zero or inversion. Victim declarations (transgender_women_excluded_from_protections, intersex_people, dsd_elite_athletes, transgender_men) derive high directionality, amplified toward the full-target end by trapped exits: documentation, career structure, and bodily history follow each of them wherever the rule operates. gender_nonconforming_cisgender_women sit mid-range — declared beneficiaries who additionally bear verification costs, which is why they carry a secondary payer role rather than a victims-array entry. The agenda-setter seats sit near the beneficiary end through collected authority rather than collected rents. Spatial scope does differential work: the sport domain runs at global scope where verification is hardest to audit, so effective extraction amplifies there most — matching the expected structural concentration of this reading's epsilon in elite sport and in sex-based violence-data policy.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is contested, not dead: sex-patterned violence, performance distributions, and clinical sex differences persist and are attested from outside the beneficiary set. But the apparatus has already undergone one mandatrophy event — blanket chromosomal testing of all female athletes died circa t24, killed by its own uselessness, and was absorbed by replacement rather than removal. The current risk vector runs the other way: as theater_ratio climbs, growing shares of the definitional apparatus are maintained for the sake of the fight rather than the function — the classic drift by which a tangled rope rots toward a piton at its symbolic margins while its medical and sporting cores stay load-bearing. The tangled_rope classification guards both mislabeling errors: calling this a rope erases the documented, concentrated harms to the excluded classes; calling it a snare erases the genuine, life-bearing coordination in clinical stratification and competitive fairness that even the constraint's fiercest critics rely on. mandatrophy_resolved is deliberately not declared — the mandate is contested, not outlived.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (sex_biology_reading) of the contested kernel woman_category — what structural changes would the sibling readings introduce, and where exactly does the disagreement between readings lie?',
    'No in-framework resolution exists: the readings coexist as live political positions. Cross-reading comparison is corpus-level measurement (each reading authors its own epsilon over the shared referent of the standing classification arrangement); adoption of a sibling reading in a jurisdiction is observable as statutory or regulatory change.',
    'Under gender_identity_reading the victim set inverts — transgender women become protected members and the enforcement burden shifts to verifying identity claims. Under intersex_accommodation_reading the ambiguity burden on intersex people dissolves and the DSD-athlete exclusion collapses. Every classification this story computes is indexical to the biology reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: one reading of the woman_category kernel; sibling deltas and the location of the disagreement (the membership criterion itself).').

omega_variable(
    typical_case_vs_boundary_enforcement,
    'Is the measured extraction produced by the typical-case definition itself, or by the enforcement apparatus built for boundary cases (sex testing, DSD regulations, documentation checks, normalization surgery)?',
    'Audit enforcement actions across jurisdictions and domains: what fraction involve atypical biology versus routine administration; compare extraction in domains with no verification machinery (vital statistics) against domains with heavy verification (elite sport, custodial placement).',
    'If extraction concentrates in boundary enforcement, the typical-case rule could persist at much lower epsilon under an accommodation protocol; if extraction is pervasive across routine administration, the definition itself carries the load and no enforcement softening removes it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(typical_case_vs_boundary_enforcement, empirical, 'Whether epsilon lives in the rule or in the machinery built to police its edges.').

omega_variable(
    coordination_extraction_separability,
    'Are the genuine coordination functions (sex-stratified medicine, competitive fairness, protective-service allocation, consistent data) separable from the exclusionary enforcement this reading performs?',
    'Natural experiments in jurisdictions operating the intersex_accommodation_reading or domain-split hybrid arrangements: do clinical outcomes, sport integrity, and service-safety indicators hold while exclusions and verification demands relax?',
    'If separable, the exclusion component is extractive overhead removable without losing the coordination; if inseparable, part of the measured epsilon is the irreducible price of the coordination itself and the tangled_rope reading is structurally forced.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability, conceptual, 'Whether the reading''s coordination and extraction components are structurally separable.').

omega_variable(
    performance_advantage_magnitude,
    'How large and how uniform is the male-puberty performance advantage that motivates the protected female category in sport — does it justify categorical exclusion at the elite level, and for which events?',
    'Longitudinal physiological and competitive-outcome studies of transgender and DSD athletes across events, sports, and transition stages, pooled across federations.',
    'A large, uniform advantage supports the sport-domain costs as the price of fairness and lowers sport-domain epsilon; event-specific or modest advantages support individualized assessment and raise the epsilon of blanket categorical rules.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(performance_advantage_magnitude, empirical, 'The empirical basis of the performance-advantage framework driving this reading''s highest-extraction domain.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(woman_category__sex_biology_reading, 0, 57).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(wc_sbr_tr_t0, woman_category__sex_biology_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(wc_sbr_tr_t0, observed).
narrative_ontology:measurement(wc_sbr_tr_t8, woman_category__sex_biology_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement_basis(wc_sbr_tr_t8, observed).
narrative_ontology:measurement(wc_sbr_tr_t16, woman_category__sex_biology_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement_basis(wc_sbr_tr_t16, observed).
narrative_ontology:measurement(wc_sbr_tr_t24, woman_category__sex_biology_reading, theater_ratio, 24, 0.28).
narrative_ontology:measurement_basis(wc_sbr_tr_t24, observed).
narrative_ontology:measurement(wc_sbr_tr_t32, woman_category__sex_biology_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement_basis(wc_sbr_tr_t32, observed).
narrative_ontology:measurement(wc_sbr_tr_t40, woman_category__sex_biology_reading, theater_ratio, 40, 0.38).
narrative_ontology:measurement_basis(wc_sbr_tr_t40, observed).
narrative_ontology:measurement(wc_sbr_tr_t48, woman_category__sex_biology_reading, theater_ratio, 48, 0.42).
narrative_ontology:measurement_basis(wc_sbr_tr_t48, observed).
narrative_ontology:measurement(wc_sbr_tr_t57, woman_category__sex_biology_reading, theater_ratio, 57, 0.46).
narrative_ontology:measurement_basis(wc_sbr_tr_t57, observed).

% Extraction over time
narrative_ontology:measurement(wc_sbr_be_t0, woman_category__sex_biology_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(wc_sbr_be_t0, observed).
narrative_ontology:measurement(wc_sbr_be_t8, woman_category__sex_biology_reading, base_extractiveness, 8, 0.42).
narrative_ontology:measurement_basis(wc_sbr_be_t8, observed).
narrative_ontology:measurement(wc_sbr_be_t16, woman_category__sex_biology_reading, base_extractiveness, 16, 0.38).
narrative_ontology:measurement_basis(wc_sbr_be_t16, observed).
narrative_ontology:measurement(wc_sbr_be_t24, woman_category__sex_biology_reading, base_extractiveness, 24, 0.44).
narrative_ontology:measurement_basis(wc_sbr_be_t24, observed).
narrative_ontology:measurement(wc_sbr_be_t32, woman_category__sex_biology_reading, base_extractiveness, 32, 0.52).
narrative_ontology:measurement_basis(wc_sbr_be_t32, observed).
narrative_ontology:measurement(wc_sbr_be_t40, woman_category__sex_biology_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement_basis(wc_sbr_be_t40, observed).
narrative_ontology:measurement(wc_sbr_be_t48, woman_category__sex_biology_reading, base_extractiveness, 48, 0.63).
narrative_ontology:measurement_basis(wc_sbr_be_t48, observed).
narrative_ontology:measurement(wc_sbr_be_t57, woman_category__sex_biology_reading, base_extractiveness, 57, 0.66).
narrative_ontology:measurement_basis(wc_sbr_be_t57, observed).

% Suppression requirement over time
narrative_ontology:measurement(wc_sbr_su_t0, woman_category__sex_biology_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement_basis(wc_sbr_su_t0, observed).
narrative_ontology:measurement(wc_sbr_su_t8, woman_category__sex_biology_reading, suppression_requirement, 8, 0.68).
narrative_ontology:measurement_basis(wc_sbr_su_t8, observed).
narrative_ontology:measurement(wc_sbr_su_t16, woman_category__sex_biology_reading, suppression_requirement, 16, 0.62).
narrative_ontology:measurement_basis(wc_sbr_su_t16, observed).
narrative_ontology:measurement(wc_sbr_su_t24, woman_category__sex_biology_reading, suppression_requirement, 24, 0.5).
narrative_ontology:measurement_basis(wc_sbr_su_t24, observed).
narrative_ontology:measurement(wc_sbr_su_t32, woman_category__sex_biology_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement_basis(wc_sbr_su_t32, observed).
narrative_ontology:measurement(wc_sbr_su_t40, woman_category__sex_biology_reading, suppression_requirement, 40, 0.6).
narrative_ontology:measurement_basis(wc_sbr_su_t40, observed).
narrative_ontology:measurement(wc_sbr_su_t48, woman_category__sex_biology_reading, suppression_requirement, 48, 0.63).
narrative_ontology:measurement_basis(wc_sbr_su_t48, observed).
narrative_ontology:measurement(wc_sbr_su_t57, woman_category__sex_biology_reading, suppression_requirement, 57, 0.67).
narrative_ontology:measurement_basis(wc_sbr_su_t57, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(woman_category__sex_biology_reading, resource_allocation).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__gender_identity_reading).
narrative_ontology:affects_constraint(woman_category__sex_biology_reading, woman_category__intersex_accommodation_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'who counts as a woman' decomposes into three structurally distinct constraints sharing the woman_category kernel, per the epsilon-invariance principle. This file instantiates the sex_biology_reading (epsilon 0.66: genuine medical, sporting, and service coordination carrying extraction concentrated on transgender women, intersex people, and DSD athletes). The gender_identity_reading (separate file) relocates the victim set entirely — transgender women become protected members and the enforcement burden shifts to verifying identity claims. The intersex_accommodation_reading (separate file) keeps the biological grounding but dissolves the ambiguity burden, collapsing the DSD-athlete exclusion. The biology reading is the historical baseline from which the siblings diverge; its enforcement controversies (mass sex testing, the Semenya line of cases) are the structural pressure that generated the accommodation reading — hence the influences edge. Domain applications where this reading's epsilon concentrates (elite-sport eligibility, violence-against-women sex-based data collection) are downstream constraints to be authored separately, each with its own epsilon.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
