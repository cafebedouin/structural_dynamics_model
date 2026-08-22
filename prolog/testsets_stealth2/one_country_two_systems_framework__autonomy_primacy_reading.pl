% ============================================================================
% CONSTRAINT STORY: one_country_two_systems_framework__autonomy_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_one_country_two_systems_framework__autonomy_primacy_reading, []).

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
 *   constraint_id: one_country_two_systems_framework__autonomy_primacy_reading
 *   human_readable: One Country Two Systems Framework — Autonomy-Primacy Reading (Treaty-Guaranteed Autonomy)
 *   domain: constitutional/political/sovereignty
 *
 * SUMMARY:
 *   The standing arrangement under assessment is the One Country, Two Systems
 *   framework as actually operated from the 1997 handover through 2025: the
 *   Basic Law's guarantees of autonomy, civil liberties, and judicial
 *   independence, together with the accumulated practice of Standing
 *   Committee interpretations, the 2020 national security law imposed without
 *   a local vote, the 2021 electoral overhaul, and the 2024 local Article 23
 *   legislation. This story instantiates the autonomy-primacy reading of that
 *   framework, which holds the treaty guarantees binding and internationally
 *   enforceable; assessed by that reading's own lights, each intervention
 *   narrowing autonomy or liberty is a treaty violation, and the
 *   arrangement's record of such interventions measures what it takes from
 *   the parties subject to it. Claim and metrics are authored independently:
 *   the reading asserts that civil liberties remain low-cost for most
 *   residents and that the democratic reform pathway stays open; the metrics
 *   describe the arrangement's actual operation as this reading evaluates it,
 *   including the concentration of penalties on political and journalistic
 *   seats and the closing of the reform pathway after 2021. Any divergence
 *   between the reading's claim and the computed classification is the datum
 *   this story exists to contribute. KEY AGENTS (by structural relationship):
 *   - prc_central_authorities: Primary beneficiary and agenda-setter
 *   (institutional/arbitrage) — controls the framework's operative meaning
 *   and collects the governance authority it redistributes -
 *   pro_democracy_politicians_activists: Primary target (powerless/trapped) —
 *   bears prosecution, disqualification, imprisonment -
 *   independent_journalists_media: Concentrated target (moderate/trapped) —
 *   asset seizure, closure, imprisonment of editors -
 *   ordinary_hong_kong_residents: Diffuse partial target
 *   (powerless/constrained) — liberty chill and emigration pressure against
 *   unchanged economic life - hong_kong_judiciary: Institutional
 *   dual-positioned seat (institutional/identity_locked) — administers the
 *   order while its review function is overridden -
 *   hong_kong_pro_establishment_elites: Secondary beneficiary
 *   (powerful/arbitrage) — staffs the vetted institutions -
 *   national_security_apparatus: Enforcement beneficiary
 *   (institutional/identity_locked) — careers constituted by the mission -
 *   united_kingdom_government: Excluded co-guarantor
 *   (institutional/arbitrage) — declares breach, holds no enforcement seat -
 *   hong_kong_diaspora_advocates: Excluded external critics
 *   (organized/mobile) - international_business_community: Beneficiary
 *   (institutional/arbitrage) — purchases stability, declines to defend the
 *   political guarantees
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, 0.88).
domain_priors:suppression_score(one_country_two_systems_framework__autonomy_primacy_reading, 0.89).
domain_priors:theater_ratio(one_country_two_systems_framework__autonomy_primacy_reading, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, extractiveness, 0.88).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 0.89).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 0.61).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(one_country_two_systems_framework__autonomy_primacy_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(one_country_two_systems_framework__autonomy_primacy_reading, tangled_rope).
narrative_ontology:human_readable(one_country_two_systems_framework__autonomy_primacy_reading, "One Country Two Systems Framework — Autonomy-Primacy Reading (Treaty-Guaranteed Autonomy)").
narrative_ontology:topic_domain(one_country_two_systems_framework__autonomy_primacy_reading, "constitutional/political/sovereignty").

domain_priors:requires_active_enforcement(one_country_two_systems_framework__autonomy_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(one_country_two_systems_framework__autonomy_primacy_reading, '9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b').
narrative_ontology:cs_kernel_codification('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', fixed_text).
narrative_ontology:cs_authority_grounding('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', lineage).
narrative_ontology:cs_interpretation_layer_present('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b').
narrative_ontology:cs_reading_relation('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', one_country_two_systems_framework__sovereignty_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', one_country_two_systems_framework__balanced_coexistence_reading, coexists_with).
narrative_ontology:cs_axiom('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', foundational, joint_declaration_binding_on_sovereign_organs).
narrative_ontology:cs_axiom_status(joint_declaration_binding_on_sovereign_organs, holdable).
narrative_ontology:cs_axiom_grounding('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', joint_declaration_binding_on_sovereign_organs, conventional).
narrative_ontology:cs_axiom('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', foundational, basic_law_rights_justiciable_against_executive).
narrative_ontology:cs_axiom_status(basic_law_rights_justiciable_against_executive, holdable).
narrative_ontology:cs_axiom_grounding('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', basic_law_rights_justiciable_against_executive, conventional).
narrative_ontology:cs_axiom('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', secondary, universal_suffrage_ultimate_aim_operative).
narrative_ontology:cs_axiom_status(universal_suffrage_ultimate_aim_operative, holdable).
narrative_ontology:cs_axiom_grounding('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', universal_suffrage_ultimate_aim_operative, conventional).
narrative_ontology:cs_reference_frame('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', treaty_guaranteed_substantive_autonomy).
narrative_ontology:cs_drift_state('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', post_national_security_law_era, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('9c92dd87-79bc-4da2-b9fc-d5ad1cf4261b', '').
narrative_ontology:cs_kernel_id(one_country_two_systems_framework__autonomy_primacy_reading, one_country_two_systems_framework).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_pro_establishment_elites).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, national_security_apparatus).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, international_business_community).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_politicians_activists).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, independent_journalists_media).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, ordinary_hong_kong_residents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary).
narrative_ontology:constraint_beneficiary(one_country_two_systems_framework__autonomy_primacy_reading, ordinary_hong_kong_residents).
narrative_ontology:constraint_victim(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_diaspora_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Drafted the Basic Law and retains sole power to interpret it through the Standing Committee. Issues interpretations that settle politically sensitive questions, imposed the 2020 national security law directly without a local vote, and rebuilt the electoral system in 2021 to guarantee loyalist majorities. Declares all of this consistent with the framework's promises. Controls appointment of the Chief Executive and senior officials. No external body reviews these acts; the framework's operative meaning is whatever these organs say it is.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities, agenda_setter,
    institutional, generational, arbitrage, global).

% Business families, professional bodies, and loyalist politicians who fill the Election Committee and Legislative Council under the vetted system. Receive office, policy protection, and contracts aligned with mainland integration plans. Many hold foreign residency or assets abroad, giving them personal mobility whatever happens locally. Their position depends on continued alignment with the central authorities.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_pro_establishment_elites, beneficiary,
    powerful, generational, arbitrage, regional).

% The police national security department, the Office for Safeguarding National Security, designated judges, and specialized prosecutors created since 2020. Expanded budgets, staffing, and powers, including transfer of some cases to mainland jurisdiction. Careers, promotions, and institutional purpose are now built around national security enforcement. Members cannot repudiate the mission without ending their careers.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, national_security_apparatus, beneficiary,
    institutional, biographical, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, national_security_apparatus, agenda_setter).

% Standing for election, organizing primaries, and legislating was their profession until 2020-2021. Since then: mass disqualification, the forty-seven-democrats subversion prosecution, lengthy remands, multi-year sentences, and travel bans on defendants awaiting trial. Those not imprisoned are barred from office; several went into exile. Re-entry into legal politics is effectively closed.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, pro_democracy_politicians_activists, payer,
    powerless, biographical, trapped, regional).

% Ran the city's critical press: Apple Daily, Stand News, Citizen News. Founders and editors arrested under collusion and sedition provisions; Apple Daily's assets were frozen and the paper forced to close in 2021; its publisher's trial proceeds without a jury. Remaining outlets practice documented self-censorship. Individuals can relocate abroad, but the institutions themselves were dismantled in place.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, independent_journalists_media, payer,
    moderate, biographical, trapped, regional).

% Administers the common-law system that anchors the city's commercial life: thousands of contracts, arbitrations, and civil disputes proceed normally. Since 2020 its members face designated-judge assignment for national security cases, no juries in those trials, Standing Committee interpretations that override its judgments, and public pressure campaigns against unpopular rulings. Overseas judges on the Court of Final Appeal have resigned rather than continue. Judges who stay cannot leave the bench without abandoning the institution they identify with.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, payer,
    institutional, generational, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_judiciary, beneficiary).

% Live daily economic lives largely unchanged: work, housing, markets, travel. Political expression now carries prosecution risk that did not exist five years ago; slogans, songs, bookshop stock, and social media posts have led to arrests. Roughly two hundred thousand residents emigrated between 2020 and 2023 under British overseas citizen pathways; others hold eligible passports unused. Longstanding civil society groups dissolved preemptively. Most people have no seat in any body that decides these questions.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, ordinary_hong_kong_residents, payer,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, ordinary_hong_kong_residents, beneficiary).

% Left in the emigration waves and now organize from Britain, Australia, Canada, and the United States: lobbying, parliamentary testimony, memorial events, community media. They bear the costs of uprooted careers and separated families and hold no formal role in any forum that decides Hong Kong's arrangements. Return visits carry arrest risk under extraterritorial provisions.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_diaspora_advocates, excluded,
    organized, biographical, mobile, global).
narrative_ontology:stakeholder_secondary_role(one_country_two_systems_framework__autonomy_primacy_reading, hong_kong_diaspora_advocates, payer).

% Co-signed the Joint Declaration and registered it at the United Nations. Issues six-monthly reports finding the treaty in breach, created the BN(O) visa pathway, and sanctioned individual officials. Holds no adjudicatory seat in the framework and commands no mechanism short of unilateral action to compel compliance. Its objections are recorded and without operative effect.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, united_kingdom_government, excluded,
    institutional, generational, arbitrage, global).

% Regional headquarters, banks, and trading houses that value common-law courts, capital mobility, and the currency peg. Publicly urge continuity and stability; quietly relocated some functions to Singapore as political risk repriced. Benefit from the framework's economic guarantees and have declined to spend capital defending its political ones.
narrative_ontology:constraint_stakeholder(one_country_two_systems_framework__autonomy_primacy_reading, international_business_community, beneficiary,
    institutional, biographical, arbitrage, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(one_country_two_systems_framework__autonomy_primacy_reading, prc_central_authorities).
narrative_ontology:fixing_cost_class(one_country_two_systems_framework__autonomy_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: At founding: coordinating the transfer of sovereignty over a capitalist enclave to a socialist state without war, capital flight, or mass displacement, preserving separate courts, currency, customs, and economic system inside one sovereign state. Continuously: providing the legal and financial infrastructure (common-law courts, contract enforcement, currency peg, separate customs territory) on which regional commerce runs.
% TRANSFER_FUNCTION: Moves decision-making authority over Hong Kong governance from local institutions and voters to central organs through interpretations, direct legislation, and appointment control; moves expressive and associative liberty from residents to the state through criminal provisions; moves stability assurance outward to international markets in exchange for continued capital access.
% ABSENT_VOICES: The majority of voters who consistently backed pan-democratic lists have no seated representative: their legislators resigned en masse in 2020, their primary organizers are convicted, their parties disbanded. The UK co-guarantor objects from outside with no seat. Taiwan, whose own cross-strait framework discussions reference this precedent, observes without voice. Within the vetted chambers, no member can voice the displaced majority's position without prosecution risk.
% DISAPPEARANCE_RATIONALE: Overnight disappearance forces immediate resolution of Hong Kong's status by other means: direct administration as a mainland city, renegotiated dependency, or contested sovereignty. The currency board, customs territory, court system, and headquarters economy all presuppose the framework; capital flight and diplomatic crisis would follow within days. Nothing about the arrangement is self-executing; it is held up continuously by the parties it organizes.
% FOUNDING_PROBLEM: Settling the disposition of a colonially administered capitalist territory whose lease was expiring, in a way that restored Chinese sovereignty while preserving the enclave's economic system and reassuring its population and trading partners: the peaceful handover.
% FOUNDING_PROBLEM_CORROBORATION: The Joint Declaration's own text (UN-registered, 1985) states its object: settling the question of Hong Kong's return and specifying governing policies through 2047. UK parliamentary records and the treaty's ratification history corroborate that the problem it was built to solve, the terms of the handover, was discharged in 1997. No source outside the arrangement's current beneficiaries attests that transition management remains the operative problem; the central authorities recast the founding problem as permanent reunification consolidation, a restatement no external party corroborates.
narrative_ontology:disappearance_verdict(one_country_two_systems_framework__autonomy_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(one_country_two_systems_framework__autonomy_primacy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(one_country_two_systems_framework__autonomy_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(one_country_two_systems_framework__autonomy_primacy_reading, 0.88, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(one_country_two_systems_framework__autonomy_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(one_country_two_systems_framework__autonomy_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness reaches 0.88 at interval end because the arrangement now removes, by central act, precisely what the reading holds treaty-guaranteed: candidate vetting replaces the promised electoral pathway, a directly imposed security statute criminalizes expression the Basic Law's Bill of Rights protects, and interpretations override final judgments. Suppression (0.89) is authored as a raw structural property, unscaled by power or scope: the enforcement machinery includes no-jury trials, designated judges, asset freezing, extraterritorial reach, and default bail denial, and its persistence depends on that machinery, not on participant preference. Theater_ratio (0.61) reflects a specific composition: elections convene but only among vetted candidates, the council sits without opposition, consultation exercises conclude in predetermined outcomes, while the courts' commercial and civil dockets remain genuinely functional — performative share is high but not total. Accessibility_collapse (0.72) is elevated above a typical contested construct because collective alternatives are not merely discouraged but criminalized, and the international enforcement avenue the reading relies on proved inert; it stops short of natural-law levels because costly individual exit (roughly two hundred thousand emigrations) demonstrably exists. Resistance (0.45) records the end state: organized domestic resistance was crushed after 2020, while diaspora advocacy and international criticism persist at meaningful but ineffective volume. The 2019 episode is the coalition datum: when the powerless seats briefly coordinated (general strikes, electoral sweep), the arrangement's controllers responded with the structural dismantling of coalition capacity itself — union dissolutions, party disbandments, prosecution of primary organizers — which is why the current powerless seats compute trapped rather than organized. All three series run on one shared eight-point grid so every metric is authored at every examined time point.
 *
 * PERSPECTIVAL GAP:
 *   The payer and agenda-setter seats should compute differently, and the divergence is extreme here. From the central-authorities seat the arrangement is a sovereignty framework performing as designed: stability delivered, integration advancing, each intervention a lawful exercise of reserved power. From the trapped payer seats the same years read as escalating prosecution: professions ended, institutions seized, colleagues imprisoned. The judiciary seat experiences both simultaneously — it administers a genuinely functioning commercial legal order while its constitutional-review function is overridden by the interpretive monopoly, which is why its exit option is identity_lock rather than mobility: leaving the bench means abandoning the institution these judges understand themselves to be. The ordinary-resident seat splits the difference: diffuse liberty costs and emigration pressure against materially unchanged daily economic life. The engine computes these per-seat classifications from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Central authorities sit at the beneficiary pole: they redistribute decision-making authority toward themselves and face no review (derived d near 0.0). Pro-establishment elites and the international business community collect office, contracts, and stability without running the enforcement machinery (low d). The national security apparatus both collects and administers, with career-fused identity lock pulling it deeper. Targets concentrate on the political and journalistic seats: trapped exit, prosecution exposure, institutional destruction (high d). The judiciary is pulled toward the target pole by overridden review and identity lock, tempered by its retained commercial function and continued resourcing. Ordinary residents occupy the ambiguous middle: diffuse liberty costs against preserved economic life, placing them moderately toward the target pole rather than at either extreme. No directionality_overrides are authored: the interested agents split across power levels (an institutional target, institutional beneficiaries, powerless targets, near-symmetric residents), so any single power-atom-level correction would misplace more seats than it repairs — the beneficiary/victim declarations plus exit options carry the differentiation the overrides array cannot express at atom granularity.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — managing the transfer of a leased colonial territory — was discharged in 1997, and the treaty text plus UK ratification records corroborate that from outside the beneficiary set. The arrangement persists with a successor function: integration and control. Authoring founding_problem_status=dead alongside disappearance_verdict=world_rearranges should trip the capture/zombie mismatch flag, and that is the honest signal: the framework is no longer doing the work it was built for, and the parties keeping it alive are the parties it now serves. Mandatrophy resolution here prevents two opposite mislabels. It prevents reading the retained commercial-court and currency-peg function as proof that the original coordination mandate survives — the mandate was transitional and is complete. It equally prevents reading the enforcement buildup as proof of pure predation with no coordination substrate — the courts, customs territory, and monetary arrangement remain load-bearing for the region, which is what keeps this a hybrid rather than a pure case. The trajectory is not decay toward theatrical inertia: enforcement intensity is rising, not atrophying, so the terminal risk is hardening on the extraction side, not vestigial performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_structural_delta,
    'This constraint is the autonomy_primacy_reading of the one_country_two_systems_framework kernel: how would the identical standing arrangement classify under the sibling readings?',
    'Generate the sibling stories (sovereignty_primacy_reading, balanced_coexistence_reading) over the same referent and compare per-seat classifications; the delta locates the disagreement structurally.',
    'Under the sovereignty reading the same interventions are lawful exercises of reserved power and epsilon collapses toward coordination cost, with the victim set emptied; under the balance reading they are negotiable boundary disputes rather than violations. What this story records as extraction, a sibling records as legitimate exercise — the comparison is the measurement.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_structural_delta, conceptual, 'Committer-frame omega: one kernel, three readings, divergent epsilon over one shared referent.').

omega_variable(
    treaty_enforceability_gap,
    'Is the Joint Declaration actually enforceable by any international mechanism, or is ''internationally enforceable'' an aspiration the arrangement''s operators know to be inert?',
    'Test whether any enforcement consequence followed the UK''s repeated breach determinations from 2020 onward: sanction effects, UN registration consequences, arbitration attempts, third-party state action.',
    'If unenforceable, the reading''s check-on-interference premise is theatrical, accessibility_collapse is understated, and the constraint''s coordination claim loses its enforcement leg; if some mechanism bites, the guarantee structure retains operative force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_enforceability_gap, empirical, 'Whether the treaty-guarantee axiom has any operative enforcement mechanism behind it.').

omega_variable(
    diffuse_vs_concentrated_liberty_extraction,
    'The reading claims civil liberties remain low-cost for most residents while the measurements show concentrated penalties on political and journalistic seats: does diffuse everyday-liberty retention offset concentrated political extraction in the arrangement''s classification?',
    'Panel data on perceived liberty across resident strata crossed with prosecution and arrest rates by activity type, separating the apolitical majority''s experience from the political class''s.',
    'If the offset holds, the ordinary-resident seat computes near-symmetric and the arrangement retains a coordination floor across most seats; if not, the arrangement classifies as extraction-bearing for nearly all resident seats and the reading''s baseline claim fails empirically.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diffuse_vs_concentrated_liberty_extraction, empirical, 'Whether liberty costs are concentrated on political seats or generalized across the resident population.').

omega_variable(
    judicial_independence_residual_scope,
    'Can the common-law judiciary retain genuine independence on non-political dockets indefinitely, or does national-security designation logic migrate into commercial and civil law?',
    'Track the foreign-judge resignation rate, expansion of security doctrines into economic offenses, and outcome divergence between political and commercial dockets over the next decade.',
    'Migration drives theater_ratio upward and pushes the arrangement toward pure extraction with performative legality; stable separation supports the hybrid classification''s coordination leg.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(judicial_independence_residual_scope, empirical, 'Trajectory of the judiciary''s residual independent function under designation pressure.').

omega_variable(
    reform_pathway_legality,
    'Does the Basic Law''s ''ultimate aim'' of universal suffrage (Articles 45 and 68) remain a legally operative commitment, or has the patriots-only electoral architecture extinguished it as a matter of operative law?',
    'Observe the Standing Committee''s treatment of any future reform proposal: interpretation and facilitation versus dismissal; doctrinal scholarship on whether an annulled ultimate aim can revive.',
    'If extinguished, the democratic-reform alternative is closed, accessibility_collapse rises further, and the reading''s reform-pathway claim is void; if live, a real alternative persists and the arrangement retains transitional character the current classification misses.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reform_pathway_legality, conceptual, 'Whether the promised reform pathway remains a live legal alternative or a dead letter.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(one_country_two_systems_framework__autonomy_primacy_reading, 1997, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(one__tr_t1997, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 1997, 0.14).
narrative_ontology:measurement(one__tr_t2003, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2003, 0.17).
narrative_ontology:measurement(one__tr_t2009, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2009, 0.21).
narrative_ontology:measurement(one__tr_t2014, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2014, 0.27).
narrative_ontology:measurement(one__tr_t2019, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2019, 0.31).
narrative_ontology:measurement(one__tr_t2020, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement(one__tr_t2022, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2022, 0.55).
narrative_ontology:measurement(one__tr_t2025, one_country_two_systems_framework__autonomy_primacy_reading, theater_ratio, 2025, 0.61).

% Extraction over time
narrative_ontology:measurement(one__be_t1997, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 1997, 0.34).
narrative_ontology:measurement(one__be_t2003, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2003, 0.41).
narrative_ontology:measurement(one__be_t2009, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2009, 0.47).
narrative_ontology:measurement(one__be_t2014, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2014, 0.57).
narrative_ontology:measurement(one__be_t2019, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2019, 0.63).
narrative_ontology:measurement(one__be_t2020, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2020, 0.77).
narrative_ontology:measurement(one__be_t2022, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2022, 0.84).
narrative_ontology:measurement(one__be_t2025, one_country_two_systems_framework__autonomy_primacy_reading, base_extractiveness, 2025, 0.88).

% Suppression requirement over time
narrative_ontology:measurement(one__su_t1997, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 1997, 0.3).
narrative_ontology:measurement(one__su_t2003, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2003, 0.37).
narrative_ontology:measurement(one__su_t2009, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2009, 0.43).
narrative_ontology:measurement(one__su_t2014, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2014, 0.54).
narrative_ontology:measurement(one__su_t2019, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2019, 0.66).
narrative_ontology:measurement(one__su_t2020, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2020, 0.79).
narrative_ontology:measurement(one__su_t2022, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2022, 0.85).
narrative_ontology:measurement(one__su_t2025, one_country_two_systems_framework__autonomy_primacy_reading, suppression_requirement, 2025, 0.89).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(one_country_two_systems_framework__autonomy_primacy_reading, resource_allocation).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, sovereignty_primacy_reading).
narrative_ontology:affects_constraint(one_country_two_systems_framework__autonomy_primacy_reading, balanced_coexistence_reading).

% DUAL FORMULATION NOTE:
% 'One Country, Two Systems' is a colloquial label covering three structurally distinct claims about the same texts; per the epsilon-invariance principle each is authored as its own story with its own beneficiaries, victims, and epsilon over the shared referent (the arrangement's actual operation, 1997-2025). This file is the autonomy-primacy member. The sovereignty-primacy member records the same interventions as lawful reserved-power exercises, collapsing epsilon toward coordination cost and emptying the victim set; the balanced-coexistence member records them as negotiable boundary disputes routed to politics. Upstream/downstream structure: the treaty-text layer this reading rests on is cited as evidence by the balance reading and denied operative force by the sovereignty reading, so contamination propagates from this member toward both siblings when the guarantee structure's credibility moves.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
