% ============================================================================
% CONSTRAINT STORY: territorial_legitimacy__indigenous_continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_legitimacy__indigenous_continuity_reading, []).

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
 *   constraint_id: territorial_legitimacy__indigenous_continuity_reading
 *   human_readable: Territorial Legitimacy — Indigenous-Continuity Reading (1948 as Nakba)
 *   domain: political theory/international law/territorial sovereignty
 *
 * SUMMARY:
 *   This story instantiates the indigenous-continuity reading of the
 *   territorial-legitimacy kernel as a single epsilon-invariant constraint.
 *   The standing arrangement under contest is the sovereignty-and-control
 *   order established over historic Palestine from 1948 onward: state
 *   founding amid mass displacement, absentee-property transfer, and, from
 *   1967, military occupation and settlement. Read by this reading's own
 *   lights, the arrangement is a dispossession structure — it coordinates
 *   governance and security for one population while excluding another from
 *   return, residency, and self-determination. Epsilon's referent is that
 *   standing arrangement, never the decolonized arrangement this reading
 *   endorses. The claimed type (snare) is asserted from the reading's
 *   structural analysis; the metrics are authored descriptively; the engine
 *   computes per-seat classifications independently, and any divergence
 *   between claim and computation is the datum. The sibling readings
 *   (partition, security-necessity) are separate constraint files linked
 *   through network.affects_constraints, not positions described inside this
 *   one.
 *
 * KEY AGENTS:
 *   - israeli_state_apparatus: Agenda-setting sovereign ([institutional]/[arbitrage]) — authors and enforces the rules of residence, return, and land
 *   - israeli_citizenry: Primary beneficiary ([organized]/[mobile]) — collects sovereignty, land, housing, and security
 *   - great_power_patrons: Secondary beneficiary and co-agenda-setter ([institutional]/[arbitrage]) — subsidizes enforcement, collects alliance value
 *   - palestinian_1948_refugees: Primary target ([powerless]/[trapped]) — bears dispossession and denial of return across generations
 *   - west_bank_palestinians: Occupied target ([moderate]/[trapped]) — bears direct permit-and-checkpoint control
 *   - gaza_strip_residents: Blockaded target ([powerless]/[trapped]) — bears closure and recurrent military operations
 *   - palestinian_citizens_of_israel: Subordinated members ([moderate]/[constrained]) — formal inclusion alongside structural land loss
 *   - arab_host_states: Cost-bearing neighbors ([institutional]/[constrained]) — absorb the arrangement's externalities
 *   - international_legal_institutions: Analytical observer ([institutional]/[analytical]) — records legality without enforcement power
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, 0.9).
domain_priors:suppression_score(territorial_legitimacy__indigenous_continuity_reading, 0.9).
domain_priors:theater_ratio(territorial_legitimacy__indigenous_continuity_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(territorial_legitimacy__indigenous_continuity_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_legitimacy__indigenous_continuity_reading, snare).
narrative_ontology:human_readable(territorial_legitimacy__indigenous_continuity_reading, "Territorial Legitimacy — Indigenous-Continuity Reading (1948 as Nakba)").
narrative_ontology:topic_domain(territorial_legitimacy__indigenous_continuity_reading, "political theory/international law/territorial sovereignty").

domain_priors:requires_active_enforcement(territorial_legitimacy__indigenous_continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_legitimacy__indigenous_continuity_reading, '8834bf18-7bd5-438a-913d-907178d33223').
narrative_ontology:cs_kernel_codification('8834bf18-7bd5-438a-913d-907178d33223', distributed).
narrative_ontology:cs_authority_grounding('8834bf18-7bd5-438a-913d-907178d33223', distributed).
narrative_ontology:cs_reading_relation('8834bf18-7bd5-438a-913d-907178d33223', territorial_legitimacy__partition_reading, forecloses).
narrative_ontology:cs_reading_relation('8834bf18-7bd5-438a-913d-907178d33223', territorial_legitimacy__security_necessity_reading, forecloses).
narrative_ontology:cs_axiom('8834bf18-7bd5-438a-913d-907178d33223', foundational, continuous_habitation_grounds_sovereignty).
narrative_ontology:cs_axiom_status(continuous_habitation_grounds_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('8834bf18-7bd5-438a-913d-907178d33223', continuous_habitation_grounds_sovereignty, deontological).
narrative_ontology:cs_axiom('8834bf18-7bd5-438a-913d-907178d33223', foundational, settler_colonial_founding_voids_state_legitimacy).
narrative_ontology:cs_axiom_status(settler_colonial_founding_voids_state_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('8834bf18-7bd5-438a-913d-907178d33223', settler_colonial_founding_voids_state_legitimacy, deontological).
narrative_ontology:cs_axiom('8834bf18-7bd5-438a-913d-907178d33223', secondary, right_of_return_is_constitutive_not_charitable).
narrative_ontology:cs_axiom_status(right_of_return_is_constitutive_not_charitable, holdable).
narrative_ontology:cs_axiom_grounding('8834bf18-7bd5-438a-913d-907178d33223', right_of_return_is_constitutive_not_charitable, deontological).
narrative_ontology:cs_reference_frame('8834bf18-7bd5-438a-913d-907178d33223', pre_1948_indigenous_demographic_order).
narrative_ontology:cs_drift_state('8834bf18-7bd5-438a-913d-907178d33223', contemporary, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('8834bf18-7bd5-438a-913d-907178d33223', '').
narrative_ontology:cs_kernel_id(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_citizenry).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, great_power_patrons).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_1948_refugees).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, west_bank_palestinians).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, gaza_strip_residents).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_victim(territorial_legitimacy__indigenous_continuity_reading, arab_host_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_of_israel).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, effective_control_title_doctrine).
narrative_ontology:constraint_vindicates(territorial_legitimacy__indigenous_continuity_reading, fait_accompli_recognition_practice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Maintains the sovereign, legal, and military machinery that decides who may reside, build, work, and move within the territory it controls: citizenship and immigration law, land registries, planning committees, permit regimes, and the army that enforces them. It writes and rewrites the rules of residence and return, and no external body currently compels reversal. Its exit from the arrangement would mean dismantling the state's own founding structure.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, israeli_state_apparatus, beneficiary).

% Lives inside the arrangement as full members: votes, owns, builds, and moves freely within the pre-1967 lines, draws state services, and enjoys security guaranteed by the same machinery that bars others from returning. Individual emigration is open and dual passports are common, but collective life, family, and property are anchored in the arrangement.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, israeli_citizenry, beneficiary,
    organized, biographical, mobile, regional).

% Supplies the military aid, diplomatic shielding, and financial flows that keep the arrangement inexpensive to maintain, and in exchange receives basing access, intelligence cooperation, and a dependable regional ally. It can reshape the arrangement's parameters quickly by conditioning assistance and occasionally does, but has declined to force reversal of the founding facts.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, great_power_patrons, beneficiary,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, great_power_patrons, agenda_setter).

% Descendants of the roughly 700,000 displaced in 1948, registered with UNRWA across camps in Lebanon, Syria, Jordan, the West Bank, and Gaza. Most hold no citizenship of the state that displaced them and are legally barred from returning to the towns and villages named in their family documents. Leaving means further dispersion; staying means inherited statelessness and camp life.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_1948_refugees, payer,
    powerless, generational, trapped, global).

% Live under a layered permit-and-checkpoint system that governs where they may travel, build, farm, and work, while settlements expand on the hilltops above their towns. The Palestinian Authority administers civil affairs in fragments but controls neither borders nor water quotas nor settlement growth. Residency permits lapse with prolonged absence, so leaving the territory forfeits the right to be there at all.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, west_bank_palestinians, payer,
    moderate, biographical, trapped, regional).

% Confined behind a blockade that limits imports, exports, fishing range, and movement, punctuated by large-scale military operations. Roughly half are refugees from 1948 towns now inside Israel. There is no routine legal route out; the crossings open and close at the discretion of the controlling states.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, gaza_strip_residents, payer,
    powerless, immediate, trapped, regional).

% Hold Israeli citizenship and vote, but a large minority were internally displaced in 1948 and lost property to the custodian of absentee assets; planning regimes have long restricted the growth of their towns while neighboring towns expanded. Emigration is possible but severs them from the only polity where they hold formal standing.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_of_israel, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(territorial_legitimacy__indigenous_continuity_reading, palestinian_citizens_of_israel, beneficiary).

% Carry the fiscal and demographic weight of decades of refugeehood: camp services, school systems, and in Lebanon's case legal exclusions barring refugees from many professions. Their leverage over the arrangement is real but episodic, spent in wars and peace initiatives, and their regimes disagree sharply on whether mass return would stabilize or destabilize them.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, arab_host_states, payer,
    institutional, generational, constrained, regional).

% Court and assembly organs that issue advisory opinions, resolutions, commissions of inquiry, and registry updates on the legality of occupation, settlements, and barriers. They compile the evidentiary record and fix legal categories, but hold no enforcement arm capable of reversing facts on the ground.
narrative_ontology:constraint_stakeholder(territorial_legitimacy__indigenous_continuity_reading, international_legal_institutions, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(territorial_legitimacy__indigenous_continuity_reading, israeli_citizenry).
narrative_ontology:fixing_cost_class(territorial_legitimacy__indigenous_continuity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a single sovereign apparatus that organizes security, land allocation, water, electricity, courts, and civil registration for the population it recognizes as its citizenry, replacing the pre-state patchwork of Ottoman land tenure, British mandate administration, and village-level governance with one integrated system of territorial management.
% TRANSFER_FUNCTION: Moves land, housing, water shares, residency rights, and the capacity for collective self-government away from the displaced and occupied Palestinian population and toward the Israeli state and its citizen body; moves the risks and costs of policing the arrangement onto the occupied population; and channels international donor funds toward camp services administered through UNRWA and the Palestinian Authority.
% ABSENT_VOICES: The 1948 refugees and their descendants — the population whose dispossession defines the arrangement — had no seat in the Oslo-era negotiating structures that produced the interim order; their representatives were neither signatories nor principals. Camp committees, host-state governments, and diaspora organizations speak from outside the room; inside it, return was deferred to final-status talks that never concluded.
% DISAPPEARANCE_RATIONALE: Citizenship status for roughly nine million Israelis, property title for land transferred after 1948, the residency of some five million occupied Palestinians, UNRWA's mandate, the Egypt and Jordan peace treaties, and the regional security architecture are all organized around this arrangement; its overnight removal would force simultaneous renegotiation of every one of them.
% FOUNDING_PROBLEM: The movement that built the arrangement sought a sovereign national home and physical safety for Jews after centuries of European persecution culminating in the Holocaust, and pursued that home in Palestine, where it collided with an existing indigenous majority.
% FOUNDING_PROBLEM_CORROBORATION: The underlying persecution problem is corroborated by an enormous record external to any benefiting party — Holocaust historiography, European archives, contemporaneous diplomatic correspondence. Whether that problem remains live, and whether it can legitimately be answered at the expense of the indigenous population, is attested only by the arrangement's beneficiaries; the reading's carriers, host states, and human-rights bodies dispute both, and no neutral arbiter currently adjudicates the dispute.
narrative_ontology:disappearance_verdict(territorial_legitimacy__indigenous_continuity_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_legitimacy__indigenous_continuity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_legitimacy__indigenous_continuity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(territorial_legitimacy__indigenous_continuity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_legitimacy__indigenous_continuity_reading, 0.9, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_legitimacy__indigenous_continuity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_legitimacy__indigenous_continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.90 because, on this reading's accounting, nearly the whole surplus of the territory — land, water, sovereignty, and the right of return — flows away from the indigenous population and accumulates on the other side. Suppression is authored at 0.90 because the arrangement persists through permit regimes, closures, blockades, and military force rather than through the consent of the governed; note suppression is a raw structural property, unscaled by power or scope — only extractiveness is scaled, by directionality and spatial scope, in the engine's computation. Theater ratio 0.44: a substantial share of observable activity is performative diplomacy — processes convened without transfer of control — layered over functional administration. Accessibility collapse 0.72: once the arrangement is understood, the alternatives (return, equal citizenship) are foreclosed in practice by law, walls, and residency rules, though they remain politically imaginable. Resistance 0.75: sustained uprising, boycott, litigation, and armed resistance recur across generations. The temporal series run on one shared ten-point grid (every tracked metric authored at every point). The trajectory is not cyclical but a relaxation-rehardening sequence: the Oslo years show a temporary dip in measured suppression and a spike in theater (ceremony without transfer), reversed after 2000 as enforcement infrastructure hardened — barrier construction, blockade, settlement acceleration. Coalition note: the payer seats span five groups whose coalition potential — host-state leverage, boycott coordination, refugee-demanded unity — is repeatedly latent but chronically unrealized, fragmented by divergent host-state interests and intra-Palestinian factional splits; the arrangement's durability owes much to that fragmentation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the agenda-setter seat the arrangement is self-authored governance maintained at real cost; from the beneficiary citizen seat it is ordinary civic life; from the five payer seats it is dispossession enforced daily. Same-level lateral divergence matters too: palestinian_citizens_of_israel and west_bank_palestinians are one people under two different legal seats, with different exits (constrained citizenship versus lapsed-residency trap), so identical global standing yields different computed positions. Institutionally, the state apparatus holds arbitrage-grade rule-writing power while international legal institutions hold analytical power with no enforcement arm — the same documents bind one seat and merely inform the other. The engine derives this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries (israeli_citizenry, israeli_state_apparatus, great_power_patrons) sit near the beneficiary end: the arrangement subsidizes them, and the citizenry's mobility plus the patrons' arbitrage push them further from the target pole. Declared victims (the five payer groups) sit near the target end, amplified by trapped exits: refugees cannot return, Gazans cannot leave, West Bank residency lapses with absence. Identity-lock binds the refugee seat specifically — the right of return is constitutive of the refugee's political identity, so exercising the available exit (further dispersion, host-state citizenship where offered) would dissolve the claim itself; for the citizenry, the state's security narrative fuses with personal safety, making the arrangement feel like protection rather than position. Spatial scope is regional-to-global (diaspora, host states, patron networks), which raises verification difficulty and amplifies effective extraction on the trapped targets. No directionality overrides are authored: the beneficiary/victim declarations plus exit atoms already produce the correct relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two mislabels. As rope: the arrangement does solve real coordination problems — water grids, courts, security, civil registration — but the same structure that coordinates the included population excludes the displaced, so the coordination function is inseparable from the transfer function; labeling it rope launders the transfer as overhead. As scaffold: nothing in the arrangement is transitional — no sunset mechanism exists, and each decade entrenches title further; the peace process performs transitionality without committing to it, which is precisely why theater ratio is authored high rather than the arrangement being read as scaffolding. On the genealogy: the founding problem (Jewish safety after European persecution) is historically real and externally corroborated, but its status as ongoing justification is contested — the arrangement persists past the point where its founding rationale commands assent outside the beneficiary set. That contested-status-plus-world-rearranges combination is the configuration the zombie/capture flag watches; this story surfaces it honestly rather than resolving it by assertion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of the territorial_legitimacy kernel — the indigenous-continuity reading. The partition reading (legitimacy via UN Resolution 181 and state recognition) and the security-necessity reading (legitimacy via defensive territorial control) instantiate different constraints over the same territory, with different victim sets, different epsilon, and different classifications. Which reading governs?',
    'No empirical test resolves a constitutive contest; resolution occurs when a party, court, or negotiated settlement adopts one reading as operative. Track which reading international bodies and the parties themselves treat as authoritative in practice.',
    'Under the partition reading the standing arrangement''s epsilon falls sharply (recognized statehood within bounded lines); under this reading it is near-maximal (settler-colonial dispossession). Classification of the identical territory flips wholesale with the reading adopted.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Kernel-membership omega: this story is one of three competing readings of territorial legitimacy.').

omega_variable(
    status_of_1948_displacement,
    'Where the readings disagree is located in the normative status of the 1948 displacement: catastrophe that voids title (this reading), lawful founding under an international partition plan (partition reading), or background fact superseded by subsequent defensive necessity (security reading). Which characterization of the same event is correct?',
    'Historiographic and jurisprudential convergence on the 1947-49 archival record — expulsions versus flight, the intent of Plan Dalet, the contemporaneous legal status of UNGA Resolution 181 — could narrow the disagreement, though normative weighting will remain contested.',
    'If the displacement is judged lawful founding, this reading loses its foundational axiom and collapses toward the partition reading; if catastrophe, the partition reading''s legitimacy claim is void at origin and this reading''s remedy becomes the only coherent one.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(status_of_1948_displacement, conceptual, 'The location of the kernel disagreement: the normative status of the 1948 displacement.').

omega_variable(
    habitation_vs_recognition_priority,
    'Can continuous-habitation legitimacy be overridden by subsequent international recognition, or does recognition of a dispossession-founded state fail to cure the defect?',
    'Comparative analysis of decolonization cases where recognition followed conquest or settlement (Algeria, Namibia, Western Sahara) and how international law treated recognition of effective control.',
    'If recognition cures, this reading reduces to a moral-protest position inside the partition frame; if it does not, the partition reading''s central instrument (recognition) is inert against this reading''s objection and the family hierarchy inverts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(habitation_vs_recognition_priority, conceptual, 'Priority conflict between habitation-derived and recognition-derived legitimacy.').

omega_variable(
    settler_colonial_category_fit,
    'Does the 1948 founding satisfy the analytic criteria of settler colonialism — elimination logic, population replacement, wholesale land-regime restructuring — sufficiently to carry this reading''s central category?',
    'Scholarly synthesis of the demographic, land-title, and archival record against the definitional literature; ICJ advisory findings and UN treaty-body conclusions on the post-1967 regime bear on the category''s extension.',
    'If the category fits loosely, the reading migrates toward a minority-rights and restitution frame with lower structural stakes; if tightly, the decolonization remedy (dissolution of settler title) follows and the right of return becomes the load-bearing demand.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(settler_colonial_category_fit, conceptual, 'Whether settler-colonial is the correct structural category for the founding.').

omega_variable(
    return_absorption_feasibility,
    'Is physical implementation of the right of return for millions of descendants compatible with any continuing civic order in the territory, or only with full replacement of the standing arrangement?',
    'Demographic absorption modeling, property-restoration registries, and comparative repatriation programs (Bosnia Annex VII) tested against actual return-uptake rates.',
    'High feasibility supports treating the remedy as transitional repair within a managed sequence; low feasibility pushes the reading toward wholesale-replacement demands and raises the stakes of every classification computed from this story.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(return_absorption_feasibility, empirical, 'Implementability of the reading''s central remedy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_legitimacy__indigenous_continuity_reading, 1948, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1948, 0.14).
narrative_ontology:measurement(terr_tr_t1956, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1956, 0.17).
narrative_ontology:measurement(terr_tr_t1967, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1967, 0.21).
narrative_ontology:measurement(terr_tr_t1978, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1978, 0.26).
narrative_ontology:measurement(terr_tr_t1993, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 1993, 0.41).
narrative_ontology:measurement(terr_tr_t2002, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2002, 0.34).
narrative_ontology:measurement(terr_tr_t2007, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2007, 0.32).
narrative_ontology:measurement(terr_tr_t2014, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2014, 0.37).
narrative_ontology:measurement(terr_tr_t2020, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2020, 0.43).
narrative_ontology:measurement(terr_tr_t2026, territorial_legitimacy__indigenous_continuity_reading, theater_ratio, 2026, 0.44).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1948, 0.78).
narrative_ontology:measurement(terr_be_t1956, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1956, 0.76).
narrative_ontology:measurement(terr_be_t1967, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1967, 0.8).
narrative_ontology:measurement(terr_be_t1978, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1978, 0.83).
narrative_ontology:measurement(terr_be_t1993, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 1993, 0.82).
narrative_ontology:measurement(terr_be_t2002, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2002, 0.85).
narrative_ontology:measurement(terr_be_t2007, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2007, 0.87).
narrative_ontology:measurement(terr_be_t2014, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2014, 0.88).
narrative_ontology:measurement(terr_be_t2020, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2020, 0.89).
narrative_ontology:measurement(terr_be_t2026, territorial_legitimacy__indigenous_continuity_reading, base_extractiveness, 2026, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1948, 0.71).
narrative_ontology:measurement(terr_su_t1956, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1956, 0.69).
narrative_ontology:measurement(terr_su_t1967, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1967, 0.75).
narrative_ontology:measurement(terr_su_t1978, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1978, 0.77).
narrative_ontology:measurement(terr_su_t1993, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 1993, 0.73).
narrative_ontology:measurement(terr_su_t2002, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2002, 0.85).
narrative_ontology:measurement(terr_su_t2007, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2007, 0.88).
narrative_ontology:measurement(terr_su_t2014, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2014, 0.87).
narrative_ontology:measurement(terr_su_t2020, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2020, 0.89).
narrative_ontology:measurement(terr_su_t2026, territorial_legitimacy__indigenous_continuity_reading, suppression_requirement, 2026, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_legitimacy__indigenous_continuity_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__partition_reading).
narrative_ontology:affects_constraint(territorial_legitimacy__indigenous_continuity_reading, territorial_legitimacy__security_necessity_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'the legitimacy of Israel/Palestine' decomposes into three structurally distinct constraints sharing one kernel, per the epsilon-invariance principle. This member carries the highest epsilon (the standing arrangement read as settler-colonial dispossession); the partition reading carries low epsilon for the recognized core and contested epsilon for the occupied territories; the security reading prices legitimacy in defensive terms. In citation practice the partition reading is upstream — international recognition is cited as having settled the question — and this reading treats that citation as exactly the move it rejects, so the family edges run analytically in both directions even though affects_constraints lists only this story's outgoing links. Each member has its own epsilon, victim set, and stakeholders; none hedges across the others.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
