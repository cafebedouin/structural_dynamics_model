% ============================================================================
% CONSTRAINT STORY: jewish_sovereignty_palestine__post_zionist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_sovereignty_palestine__post_zionist_reading, []).

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
 *   constraint_id: jewish_sovereignty_palestine__post_zionist_reading
 *   human_readable: Post-Zionist Reading: The Ethnic-National State Framework as Obstruction to Civic Equality and Regional Integration
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates the POST-ZIONIST READING of the kernel
 *   jewish_sovereignty_palestine: the Zionist project achieved statehood, and
 *   the live question is what its founding narrative and ethnic-national
 *   framework DO NOW. The referent of epsilon is the standing arrangement
 *   under contest — the existing ethnic-national state framework as it
 *   operates (Law of Return, land-access asymmetries, national institutions,
 *   the occupation administration, the Jewish-and-democratic formula) —
 *   assessed by this reading's own lights. It is NOT the de-Zionized civic
 *   arrangement this reading endorses; authoring epsilon against the endorsed
 *   alternative would drive it to zero for every advocacy reading and destroy
 *   the measurement. The claim/metric gap is deliberate: the reading CLAIMS
 *   tangled_rope (genuine coordination function plus asymmetric extraction
 *   plus active enforcement) while the metrics are authored independently
 *   from the observed operation. Assumptions stated: the interval maps
 *   integer time points to years since 1948 (T=0 is 1948, T=76 is 2024);
 *   extraction and suppression figures aggregate citizen-line and
 *   occupation-line effects into one scalar, with the occupation contributing
 *   the larger share after 1967. KEY AGENTS (by structural relationship): -
 *   jewish_citizens_israel: Primary beneficiary (powerful/mobile) — collects
 *   immigration priority, land access, majority assurance; bears conscription
 *   and war costs secondarily - diaspora_jewry: Secondary beneficiary
 *   (organized/arbitrage) — holds unconditional entry option without
 *   residency - palestinian_citizens_of_israel: Primary target inside the
 *   Green Line (moderate/constrained) — formal citizenship, asymmetric
 *   allocation - west_bank_palestinians_under_occupation: Primary target
 *   under military rule (powerless/trapped) -
 *   palestinian_refugees_denied_return: Structurally excluded target
 *   (powerless/trapped) — barred by the same entry rule that privileges Jews
 *   worldwide - knesset_and_state_institutions: Agenda setter
 *   (institutional/identity_locked) — writes and enforces the framework -
 *   arab_neighboring_states: Regional payer (institutional/constrained) —
 *   carries the integration blockage - international_human_rights_bodies:
 *   Analytical observer (institutional/analytical)
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, 0.7).
domain_priors:suppression_score(jewish_sovereignty_palestine__post_zionist_reading, 0.75).
domain_priors:theater_ratio(jewish_sovereignty_palestine__post_zionist_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_sovereignty_palestine__post_zionist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_sovereignty_palestine__post_zionist_reading, tangled_rope).
narrative_ontology:human_readable(jewish_sovereignty_palestine__post_zionist_reading, "Post-Zionist Reading: The Ethnic-National State Framework as Obstruction to Civic Equality and Regional Integration").
narrative_ontology:topic_domain(jewish_sovereignty_palestine__post_zionist_reading, "political philosophy/nationalism studies/postcolonial theory").

domain_priors:requires_active_enforcement(jewish_sovereignty_palestine__post_zionist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_sovereignty_palestine__post_zionist_reading, '3e4976c2-f334-4093-a2f6-a09848503e3d').
narrative_ontology:cs_kernel_codification('3e4976c2-f334-4093-a2f6-a09848503e3d', fixed_text).
narrative_ontology:cs_authority_grounding('3e4976c2-f334-4093-a2f6-a09848503e3d', lineage).
narrative_ontology:cs_interpretation_layer_present('3e4976c2-f334-4093-a2f6-a09848503e3d').
narrative_ontology:cs_reading_relation('3e4976c2-f334-4093-a2f6-a09848503e3d', jewish_sovereignty_palestine__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('3e4976c2-f334-4093-a2f6-a09848503e3d', jewish_sovereignty_palestine__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e4976c2-f334-4093-a2f6-a09848503e3d', jewish_sovereignty_palestine__religious_zionist_reading, coexists_with).
narrative_ontology:cs_reading_relation('3e4976c2-f334-4093-a2f6-a09848503e3d', jewish_sovereignty_palestine__cultural_zionist_reading, influences).
narrative_ontology:cs_axiom('3e4976c2-f334-4093-a2f6-a09848503e3d', foundational, foundational_task_complete_at_statehood).
narrative_ontology:cs_axiom_status(foundational_task_complete_at_statehood, holdable).
narrative_ontology:cs_axiom_grounding('3e4976c2-f334-4093-a2f6-a09848503e3d', foundational_task_complete_at_statehood, instrumental).
narrative_ontology:cs_axiom('3e4976c2-f334-4093-a2f6-a09848503e3d', foundational, civic_equality_over_ethnic_allocation).
narrative_ontology:cs_axiom_status(civic_equality_over_ethnic_allocation, holdable).
narrative_ontology:cs_axiom_grounding('3e4976c2-f334-4093-a2f6-a09848503e3d', civic_equality_over_ethnic_allocation, deontological).
narrative_ontology:cs_reference_frame('3e4976c2-f334-4093-a2f6-a09848503e3d', sovereignty_achieved_civic_equality_pending).
narrative_ontology:cs_drift_state('3e4976c2-f334-4093-a2f6-a09848503e3d', post_nation_state_law_entrenchment, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3e4976c2-f334-4093-a2f6-a09848503e3d', '').
narrative_ontology:cs_kernel_id(jewish_sovereignty_palestine__post_zionist_reading, jewish_sovereignty_palestine).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel).
narrative_ontology:constraint_beneficiary(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jewry).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians_under_occupation).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_denied_return).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel).
narrative_ontology:constraint_victim(jewish_sovereignty_palestine__post_zionist_reading, arab_neighboring_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold citizenship in a state whose basic laws define it as the nation-state of the Jewish people. Immigration priority under the Law of Return, preferential access to state land, development budgets routed through national institutions, and permanent majority assurance flow to them. They also supply the conscripted soldiery, the tax base, and the war casualties; emigration, often with dual citizenship, is open to those who choose it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel, beneficiary,
    powerful, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel, payer).

% Holds an unconditional right to immigrate and receive immediate citizenship whether or not any member ever exercises it. The guarantee functions as standing refuge insurance and anchors communal identity, philanthropy, and political mobilization abroad; most members never relocate, collecting the option without bearing residency costs.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, diaspora_jewry, beneficiary,
    organized, generational, arbitrage, global).

% About a fifth of the citizenry. They vote and sit in the legislature, but land administration, planning approvals, municipal budgets, admissions committees in hundreds of communities, family-unification rules, and the state's symbolic definition systematically allocate less to them than to Jewish citizens. Emigration is possible but means forfeiting home, community, and citizenship; staying means permanent minority status inside a state constituted by another people's nationality.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).

% Live under military administration in territory the state controls without incorporating their population: settlement expansion onto their land, a permit regime governing movement and work, parallel legal systems, and no vote over the government that rules them. Exit routes through neighboring borders and Gulf labor markets are narrow and policed.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, west_bank_palestinians_under_occupation, payer,
    powerless, biographical, trapped, regional).

% Descendants of those displaced in 1948 and 1967, living in camps and exile communities across the region. The same legal order that grants any Jew worldwide immediate citizenship bars their return to homes inside the state's territory. They hold no seat in any body that administers the arrangement; their objection is registered only from outside it.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_denied_return, excluded,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_secondary_role(jewish_sovereignty_palestine__post_zionist_reading, palestinian_refugees_denied_return, payer).

% Writes and administers the Basic Laws, the Law of Return, the Land Authority, and the budget lines that operationalize the national framework. Coalition politics runs through parties competing to deepen or defend the national-character provisions; the state's self-description as Jewish and democratic is woven into court doctrine, public ceremony, and officials' careers, so revisiting the framework from inside threatens the institutions' own sense of what they are.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, knesset_and_state_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Carry the regional consequences: closed or conditional borders, refugee-hosting burdens, past wars and boycotts, and normalization diplomacy that stalls whenever the unresolved core resurfaces. Recent bilateral accords offer partial bypass, but full regional integration remains gated on the arrangement the framework maintains.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, arab_neighboring_states, payer,
    institutional, generational, constrained, continental).

% Treaty committees, special rapporteurs, and international courts document differential treatment, occupation practices, and the return asymmetry. They publish findings and advisory opinions but command no enforcement power over the domestic framework.
narrative_ontology:constraint_stakeholder(jewish_sovereignty_palestine__post_zionist_reading, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_sovereignty_palestine__post_zionist_reading, jewish_citizens_israel).
narrative_ontology:fixing_cost_class(jewish_sovereignty_palestine__post_zionist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates Jewish national self-determination: a guaranteed ingathering channel (Law of Return), collective defense, a Hebrew-language public sphere, land development and housing allocation for the majority population, and demographic-majority assurance mechanisms.
% TRANSFER_FUNCTION: Moves land access, housing and development budgets, immigration slots, symbolic recognition, and sovereign decision-power toward Jewish citizens; moves the displacement legacy, planning restriction, permit burden, and statelessness toward Palestinians under the state's jurisdiction or claiming its territory.
% ABSENT_VOICES: palestinian_refugees_denied_return are the structurally absent voice: the entry rule that admits any Jew worldwide is administered by bodies in which they hold no seat. west_bank_palestinians_under_occupation are governed without representation in the government ruling them. palestinian_citizens_of_israel are present but permanently outvoted on framework questions.
% DISAPPEARANCE_RATIONALE: If the ethnic-national framework vanished overnight, citizenship and immigration law, land administration, coalition politics, the courts' balancing doctrine, and regional diplomacy would all reorganize: allocation rules would rewrite around civic personhood, refugee claims would reopen, and normalization processes would lose their gating condition.
% FOUNDING_PROBLEM: European antisemitism and Jewish statelessness: a dispersed people with no guaranteed refuge or collective sovereignty. The framework was built to secure a demographic-majority Jewish homeland with unrestricted ingathering.
% FOUNDING_PROBLEM_CORROBORATION: That the founding problem existed is corroborated outside the benefiting parties by the historical record of European persecution and statelessness — archives, contemporaneous diplomatic correspondence, and postwar refugee accounting no party disputes. On its CURRENT status the record splits: UN treaty-body findings and Palestinian testimony attest the solved-and-inverted reading this story instantiates; security establishments and major diaspora organizations attest continued liveness. No external source settles the question — the corroboration itself is contested, which is the signal.
narrative_ontology:disappearance_verdict(jewish_sovereignty_palestine__post_zionist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_sovereignty_palestine__post_zionist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_sovereignty_palestine__post_zionist_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_sovereignty_palestine__post_zionist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_sovereignty_palestine__post_zionist_reading, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_sovereignty_palestine__post_zionist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_sovereignty_palestine__post_zionist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is 0.70 at interval end: allocation of land, housing, budgets, immigration slots, and decision-power runs systematically along ethnic lines, decoupled from contribution or need, while the coordination functions delivered in exchange are real but ethnically distributed. Suppression is 0.75: persistence depends on active machinery — the occupation's permit and settlement administration, land-authority enforcement, family-unification restrictions — not on participant preference. Theater is 0.42 and rising monotonically: the Jewish-and-democratic formula requires ever more rhetorical and jurisprudential maintenance as the tension between its halves sharpens, so a growing share of institutional activity defends the formula rather than performs the function. Accessibility collapse is 0.55: alternatives (civic-equality reform, binational models) remain articulable and periodically tabled but are structurally outvoted and delegitimized; they are suppressed, not erased. Resistance is 0.65: sustained minority-party mobilization, litigation, protest waves, refugee advocacy, and international pressure meet the framework continuously. The temporal series run on ONE shared grid (T = 0, 15, 25, 35, 45, 55, 65, 76) with every tracked metric authored at every point. Trajectory phases: T=0 maximal displacement-era extraction and martial-law suppression; gradual citizen-line liberalization through the 1950s-60s; a suppression uptick at T=25 as the post-1967 occupation machinery builds; an Oslo-era trough at T=45; re-entrenchment from T=55 onward (second-intifada hardening, separation barrier, the 2018 Nation-State Law's constitutionalization of the national character) lifting extraction back toward 0.70 while theater climbs throughout.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently and the divergence is the datum. From the knesset_and_state_institutions seat, the framework is the self-maintenance of a legitimate national project — coordination it built, staffs, and identifies with (its identity_locked exit makes even sympathetic administrators experience reform as self-annihilation). From the palestinian_citizens_of_israel seat, the same structure is permanent minority subordination administered through routine administrative acts. From the diaspora_jewry seat it is nearly pure option value — a guarantee held at zero residency cost. From the west_bank seat it is unaccountable military rule. The engine computes these per-seat classifications from the power and exit data; nothing in the authored claim adjudicates between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: jewish_citizens_israel and diaspora_jewry sit near the beneficiary end (low d, damped or inverted effective extraction), with diaspora_jewry pushed furthest by its arbitrage-grade exit — it collects the option without bearing the arrangement's costs. Victim declarations drive the target end: palestinian_citizens_of_israel (constrained exit), west_bank_palestinians_under_occupation (trapped), and palestinian_refugees_denied_return (trapped and excluded) sit near the full-target end, with trapped and identity-locked positions amplifying effective extraction relative to mobile agents at the same nominal power. jewish_citizens_israel's secondary payer position (conscription, taxation, war exposure) tempers its d below pure beneficiary but does not displace it, because the flows the framework moves land disproportionately in its account. knesset_and_state_institutions is the agenda-setter seat: it administers the arrangement rather than merely collecting, and its identity_locked exit explains why the agenda does not move despite internal criticism. arab_neighboring_states bear diffuse regional costs with partial bilateral exits; international_human_rights_bodies observe analytically. No directionality overrides were needed: the beneficiary/victim declarations plus exit options produce the correct qualitative ordering for every seat.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification guards against two symmetric mislabels. Reading the framework as pure coordination (rope) erases the documented asymmetric allocation — the Law of Return's mirror-image denial of return, the land and budget asymmetries, the occupation's permit regime — which is not coordination overhead but directional transfer. Reading it as pure extraction (snare) erases the genuine functions the framework delivers: a working ingathering channel that absorbed millions of refugees, collective defense, a consolidated language and public sphere. The R5 interview locates the mandatrophy question precisely: the founding problem (statelessness, no refuge) is at minimum substantially addressed — statehood exists, the ingathering happened — while the machinery built for it now produces the obstruction this reading names. Because the founding_problem_status is authored contested rather than dead, the zombie flag does not fire automatically; the omega refuge_problem_liveness carries the open question. Watch the temporal signature: theater_ratio rising monotonically across the whole interval with extraction re-climbing after the Oslo trough is the drift pattern that, if the founding problem resolves as dead, converts this tangled_rope into a snare candidate; if the problem resolves as live, the coordination floor holds and the classification stabilizes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is ONE READING of kernel jewish_sovereignty_palestine (reading_id: post_zionist_reading). Which structural elements do the sibling readings dispute, and how would adopting each sibling change this story''s numbers?',
    'Cross-reading comparison across the family files: liberal_nationalist_reading treats the ethnic framework as the legitimate exercise of collective self-determination (lower epsilon, victim set recedes); settler_colonial_reading treats the entire immigration-sovereignty structure as a displacement regime regardless of intent (higher epsilon, type shifts toward snare); religious_zionist_reading grounds the framework theologically (deontological lock, no empirical resolution path); cultural_zionist_reading denies the sovereignty-plus-demography package entirely (different constraint shape altogether).',
    'Epsilon, victim set, and claimed type are reading-indexed. The kernel can only be classified by classifying each reading separately; merging readings into one story would violate epsilon-invariance and fabricate a compromise constraint no party holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one of five live readings of a contested kernel; the disagreement is located in whether the ethnic-national framework is constitutive of legitimate self-determination, divinely mandated, a colonial displacement regime, or a completed project whose narrative now obstructs equality.').

omega_variable(
    ethnic_framework_separability,
    'Can the state''s coordination functions (defense, ingathering infrastructure, services, a functioning polity) be separated from the ethnic-preference structure, or are they constitutively fused?',
    'Comparative constitutional analysis of states that transitioned from ethnic to civic nationalism, plus natural experiments from partial reform packages: if service delivery, defense capacity, and immigrant absorption survive decoupling allocation from ethnicity, the functions are separable.',
    'If separable, de-Zionization preserves the coordination function while removing the asymmetric allocation, and the constraint tracks a tangled_rope-to-rope trajectory. If fused, the measured extraction is closer to the price of the framework itself and dissolution-or-redesign, not reform, is indicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ethnic_framework_separability, empirical, 'Whether the coordination and extraction components of the ethnic-national framework are structurally separable.').

omega_variable(
    suppression_mechanism_split,
    'Is the suppression binding palestinian_citizens_of_israel structural (land regimes, planning law, budget rules, family-unification statutes) or internalized (political demobilization and learned constraint after decades of minority status)?',
    'Post-reform trajectory: if allocation equalizes after statutory change and political participation normalizes within a generation, suppression was mostly structural; persistent demobilization after barrier removal indicates internalized residue carried through exit.',
    'Internalized components raise effective suppression beyond the structural measure and persist after formal reform; purely structural components fall with statute. The proportion determines how much of the authored suppression survives any legislative fix.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_split, empirical, 'Structural versus internalized suppression mechanism for the citizen minority.').

omega_variable(
    refuge_problem_liveness,
    'Is the founding problem — Jewish statelessness and persecution requiring a guaranteed demographic-majority refuge — still live, or has statehood retired it?',
    'Independent assessment weighing contemporary antisemitism severity and alternative refuge availability against the insecurity the framework itself now generates for its own beneficiaries and neighbors; attested from seats outside the benefiting parties.',
    'If the founding problem is dead, the framework persists as privilege maintenance and mandatrophy signals (rising theater, citizen-level extraction accumulation) should strengthen toward snare or piton detection. If live, the coordination floor stays defensible and the tangled_rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refuge_problem_liveness, empirical, 'Liveness of the founding problem, the pivot on which this reading''s genealogy turns.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_sovereignty_palestine__post_zionist_reading, 0, 76).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t0, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(jewi_tr_t15, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 15, 0.2).
narrative_ontology:measurement(jewi_tr_t25, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 25, 0.25).
narrative_ontology:measurement(jewi_tr_t35, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 35, 0.3).
narrative_ontology:measurement(jewi_tr_t45, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 45, 0.34).
narrative_ontology:measurement(jewi_tr_t55, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 55, 0.37).
narrative_ontology:measurement(jewi_tr_t65, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 65, 0.4).
narrative_ontology:measurement(jewi_tr_t76, jewish_sovereignty_palestine__post_zionist_reading, theater_ratio, 76, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t0, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 0, 0.78).
narrative_ontology:measurement(jewi_be_t15, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 15, 0.72).
narrative_ontology:measurement(jewi_be_t25, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 25, 0.71).
narrative_ontology:measurement(jewi_be_t35, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 35, 0.66).
narrative_ontology:measurement(jewi_be_t45, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 45, 0.63).
narrative_ontology:measurement(jewi_be_t55, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 55, 0.67).
narrative_ontology:measurement(jewi_be_t65, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 65, 0.69).
narrative_ontology:measurement(jewi_be_t76, jewish_sovereignty_palestine__post_zionist_reading, base_extractiveness, 76, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(jewi_su_t0, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 0, 0.82).
narrative_ontology:measurement(jewi_su_t15, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 15, 0.78).
narrative_ontology:measurement(jewi_su_t25, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 25, 0.8).
narrative_ontology:measurement(jewi_su_t35, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 35, 0.74).
narrative_ontology:measurement(jewi_su_t45, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 45, 0.7).
narrative_ontology:measurement(jewi_su_t55, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 55, 0.73).
narrative_ontology:measurement(jewi_su_t65, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 65, 0.74).
narrative_ontology:measurement(jewi_su_t76, jewish_sovereignty_palestine__post_zionist_reading, suppression_requirement, 76, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_sovereignty_palestine__post_zionist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, religious_zionist_reading).
narrative_ontology:affects_constraint(jewish_sovereignty_palestine__post_zionist_reading, cultural_zionist_reading).

% DUAL FORMULATION NOTE:
% Kernel decomposition: 'Jewish sovereignty in Palestine' is one colloquial label covering at least five structurally distinct commitments. Per the epsilon-invariance principle, each reading is authored as its own constraint story with its own epsilon, beneficiary/victim structure, and classification; this file authors the post-Zionist instantiation. Family edges run through network.affects_constraints to all four siblings. The upstream/downstream gradient runs roughly: cultural_zionist (minimal claims) -> liberal_nationalist (self-determination) -> this reading (achieved-statehood critique) -> settler_colonial (root-characterization challenge), with religious_zionist orthogonal on grounding type.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
