% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__liberal_nationalist_reading, []).

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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish Self-Determination — Liberal Nationalist Reading (Equal-Nation Parity Claim)
 *   domain: political philosophy/nationalism studies/postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the kernel
 *   jewish_self_determination: the liberal nationalist claim that the Jewish
 *   people constitute a nation holding an equal entitlement to
 *   self-determination alongside other peoples. As a standing arrangement,
 *   the claim operates through recognition politics (Mandate-era
 *   codification, UNGA 181, bilateral recognition), through the
 *   partition-plus-mutual-recognition settlement formula, and through the
 *   refuge function that converts sovereignty into a rescue option for
 *   persecuted Jews. The epsilon referent is the standing arrangement under
 *   contest — the parity claim as institutionalized — assessed by the
 *   reading's own lights (equal dignity of peoples, consent-based
 *   legitimacy); it is NOT the rival arrangements sibling readings would
 *   install. The claim and the metrics are independent authored facts: the
 *   reading is CLAIMED as rope (a genuine coordination device for competing
 *   national claims, carried by persuasion within liberal premises), while
 *   the metrics describe the arrangement's actual operation, including
 *   implementation asymmetry and a rising defensive-enforcement requirement.
 *   Per the epsilon-invariance principle, the kernel label decomposes into
 *   five structurally distinct constraints (this file plus four sibling
 *   files); they are linked via network.affects_constraints, not merged here.
 *
 * KEY AGENTS:
 *   - - jewish_diaspora_communities: Principal beneficiary (organized/mobile) — receives public standing for peoplehood and the refuge option
 *   - - israeli_citizenry: Beneficiary-payer (institutional/constrained) — holds the realized sovereignty and bears its defense costs
 *   - - persecuted_jewish_refugees: Crisis-window beneficiary (powerless/trapped) — the refuge function's direct recipients
 *   - - palestinian_national_community: Primary payer (organized/trapped) — bears the implementation costs of the parity formula
 *   - - zionist_institutions: Agenda setter (institutional/identity_locked) — administers the claim's settlement and advocacy machinery
 *   - - great_power_patrons: Agenda setter (institutional/mobile) — converted the claim into operative international law
 *   - - arab_regional_states: Excluded party (institutional/mobile) — objected to the codified formula from outside the rooms where it was written
 *   - - normative_political_theorists: Analytical observer (analytical/analytical) — evaluates the claim's coherence and its costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.32).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.42).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish Self-Determination — Liberal Nationalist Reading (Equal-Nation Parity Claim)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political philosophy/nationalism studies/postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, '9896c88a-dabb-4c56-b27b-f87309dc5e98').
narrative_ontology:cs_kernel_codification('9896c88a-dabb-4c56-b27b-f87309dc5e98', distributed).
narrative_ontology:cs_authority_grounding('9896c88a-dabb-4c56-b27b-f87309dc5e98', distributed).
narrative_ontology:cs_reading_relation('9896c88a-dabb-4c56-b27b-f87309dc5e98', jewish_self_determination__indigenous_return_reading, influences).
narrative_ontology:cs_reading_relation('9896c88a-dabb-4c56-b27b-f87309dc5e98', jewish_self_determination__settler_colonial_reading, coexists_with).
narrative_ontology:cs_reading_relation('9896c88a-dabb-4c56-b27b-f87309dc5e98', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('9896c88a-dabb-4c56-b27b-f87309dc5e98', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_axiom('9896c88a-dabb-4c56-b27b-f87309dc5e98', foundational, peoples_hold_equal_self_determination_rights).
narrative_ontology:cs_axiom_status(peoples_hold_equal_self_determination_rights, holdable).
narrative_ontology:cs_axiom_grounding('9896c88a-dabb-4c56-b27b-f87309dc5e98', peoples_hold_equal_self_determination_rights, deontological).
narrative_ontology:cs_axiom('9896c88a-dabb-4c56-b27b-f87309dc5e98', foundational, secular_recognition_suffices_for_legitimacy).
narrative_ontology:cs_axiom_status(secular_recognition_suffices_for_legitimacy, holdable).
narrative_ontology:cs_axiom_grounding('9896c88a-dabb-4c56-b27b-f87309dc5e98', secular_recognition_suffices_for_legitimacy, conventional).
narrative_ontology:cs_axiom('9896c88a-dabb-4c56-b27b-f87309dc5e98', secondary, partition_resolves_competing_claims_symmetrically).
narrative_ontology:cs_axiom_status(partition_resolves_competing_claims_symmetrically, holdable).
narrative_ontology:cs_axiom_grounding('9896c88a-dabb-4c56-b27b-f87309dc5e98', partition_resolves_competing_claims_symmetrically, instrumental).
narrative_ontology:cs_reference_frame('9896c88a-dabb-4c56-b27b-f87309dc5e98', wilsonian_peoples_parity).
narrative_ontology:cs_drift_state('9896c88a-dabb-4c56-b27b-f87309dc5e98', contemporary_postcolonial_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('9896c88a-dabb-4c56-b27b-f87309dc5e98', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_communities).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, israeli_citizenry).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, persecuted_jewish_refugees).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_national_community).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, israeli_citizenry).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, wilsonian_nationality_principle).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, un_charter_peoples_equality_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, minority_nation_legibility_standard).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Live as minorities across dozens of host societies, organized through congregations, federations, and advocacy bodies. The parity principle gives their peoplehood public standing: host governments and institutions recognize Jews as a nation and not merely a religion, which shapes minority-rights claims and guarantees a place of retreat. Individual members vary widely in identification with the national project; opting out of it carries social cost inside communal institutions but no legal barrier in liberal host states.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_communities, beneficiary,
    organized, generational, mobile, global).

% Hold citizenship in the state that embodies the claim. They receive the realized good — territory, self-government, automatic immigration rights for co-nationals — and pay for its defense through conscription, reserve duty, war casualties, and security spending. Internal politics divides over how far the state should extend territory versus trade it for recognition. Emigration is legal and common enough to have a name, but language, family, army ties, and identity anchor most.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, israeli_citizenry, beneficiary,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, israeli_citizenry, payer).

% Hold a reciprocal national claim to the same territory, which this framework recognizes in principle as equal. In practice they have borne the larger share of the arrangement's costs: displacement in 1948, statelessness distributed across neighboring states and occupied territories, and a partition formula whose terms were set without their consent and whose implementation has tracked the stronger party's position. Their claim is tied to this land; there is no alternative territory to which it could migrate.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_community, payer,
    organized, generational, trapped, regional).

% In crisis windows — 1930s Europe, the postwar displaced-persons camps, the Soviet refusenik era, the Ethiopian airlifts, post-Soviet and Ukrainian emergencies — the sovereign refuge is the concrete deliverable of the principle. They arrive with nothing, are absorbed by immigration law rather than discretionary asylum, and cannot exit the circumstances that make the refuge necessary.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, persecuted_jewish_refugees, beneficiary,
    powerless, biographical, trapped, global).

% The World Zionist Organization, the Jewish Agency, and successor bodies built settlement, immigration, and advocacy machinery decades before statehood and continue administering diaspora-facing functions. They are constituted by the parity claim itself; abandoning it would dissolve their mandate. Leadership rotates, but the organizational commitment is total.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, zionist_institutions, agenda_setter,
    institutional, generational, identity_locked, global).

% Britain issued the Balfour Declaration; the United States and the Soviet Union backed partition in 1947; the United States has since supplied diplomatic cover and military assistance. Patronage is what made the parity claim operative in international law. Patrons can reprice or withdraw support — as Britain effectively did with the 1939 White Paper — and their commitment fluctuates with strategic interest rather than principle.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, great_power_patrons, agenda_setter,
    institutional, biographical, mobile, continental).

% Surrounding states opposed the partition plan militarily in 1948, absorbed Palestinian refugees (most withholding citizenship), and fought subsequent wars; several have since signed normalization agreements. They were excluded from the forums where the principle was codified and their objections never entered its text; their later bargaining power came from war and oil, not from a seat in the founding conversation.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, arab_regional_states, excluded,
    institutional, generational, mobile, regional).

% Political theorists, international-law scholars, and historians evaluate whether the parity claim coheres with liberal premises, how it compares with other national movements' claims, and what its realization has cost. They take no side in the contest, and their analyses feed every party.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, normative_political_theorists, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__liberal_nationalist_reading, israeli_citizenry).
narrative_ontology:fixing_cost_class(jewish_self_determination__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared standard for adjudicating two national claims to one territory: if peoples hold equal self-determination rights, then reciprocal recognition plus territorial partition is the fair settlement formula. It also coordinates dispersed Jewish communal action around a legible civic-national project, and gives third parties a rule for extending recognition without adjudicating theology or ancient title.
% TRANSFER_FUNCTION: Moves international recognition, legitimacy, and ultimately sovereign jurisdiction toward the Jewish national collectivity; moves the refuge option to persecuted Jews in crisis windows; and moves the costs of territorial concession and adjustment onto whichever party holds less leverage at each implementation moment — historically, the Palestinian community.
% ABSENT_VOICES: At the codifying moments (Balfour 1917, San Remo 1920, UNGA 181) the Arab inhabitants of Palestine had no consenting seat: their objection existed but never entered the principle's formulation. Contemporary Palestinian representation sits in the conversation but inherits a fixed premise. Non-Zionist and diasporist Jewish voices were present at the founding and outvoted, not absent — the genuinely absent seat was the local non-Jewish population's consent, paired with the excluded arab_regional_states stakeholder.
% DISAPPEARANCE_RATIONALE: If the parity principle vanished overnight, Jewish national claims would lose their liberal legibility — falling back on theological or historical-title arguments that persuade narrower audiences — recognition diplomacy would lose its working formula, and the refuge architecture would lose its normative warrant. The underlying facts of attachment and presence would persist, but the arrangement organizing them would rearrange around whichever rival frame seized the vacancy.
% FOUNDING_PROBLEM: Mass statelessness and recurrent persecution of a dispersed people in the nation-state era: between expulsions, pogroms, and finally the Holocaust, Jewish collective survival lacked the sovereign guarantee other nations took for granted. The founding problem was converting peoplehood into the era's currency of safety — sovereignty.
% FOUNDING_PROBLEM_CORROBORATION: Holocaust historiography and the contemporaneous diplomatic record corroborate the founding danger from outside the beneficiary set; postcolonial critics of Zionism concede the historical persecution while disputing the remedy. Whether the problem remains live is attested in one direction by security studies and community-security incident data, and disputed in the other by scholars arguing sovereignty has become the risk multiplier. No source outside the dispute neutrally certifies the status — hence 'contested' rather than 'live' or 'dead'.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).
:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.32 (low-to-moderate): the parity premise itself demands symmetry, so the principle's intrinsic demand is cheap for adherents; the measured extraction reflects three residual cost streams — the non-consensual character of the founding partition for the local population, the asymmetric implementation that has tracked the stronger party's position, and the ordinary nation-state costs borne by minorities inside the self-determining polity. Suppression 0.42: the core claim persists by persuasion (it follows from liberal premises many holders accept independently), but a defensive enforcement layer has grown since the 2000s — anti-delegitimization statutes, definitional regimes with speech consequences, funding conditionality — and the scalar records that force as currently applied. Suppression is authored as a raw structural property and is deliberately unscaled; only extractiveness is scaled by directionality and scope downstream. Theater_ratio 0.28: ceremonial reaffirmation of the claim grows as substantive progress stalls (anniversary declarations substituting for settlement), but recognition and refuge delivery remain real functions. Accessibility_collapse 0.55: within a liberal-nationalist framework, denying Jewish nationhood collapses as inconsistent once the premise is granted, yet rival readings remain fully live across frameworks — hence mid-range rather than mountain-grade collapse. Resistance 0.6: the claim meets sustained organized resistance from the Palestinian national movement, postcolonial scholarship, and portions of the diaspora left. The temporal series oscillates rather than drifting monotonically: extraction and suppression peaked after 1967, dipped during the Oslo window, and re-rose as the process collapsed — the cycle tracks diplomatic openings and closures, not intermittent reinforcement as an extraction mechanism. All three tracked metrics share one eight-point grid so no metric row is sampled against another metric's end-state.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the palestinian_national_community seat, the arrangement presents as an enforced asymmetry: a formula codified without their consent, implemented along the stronger party's line, with no territorial exit — high effective extraction. From the jewish_diaspora_communities and persecuted_jewish_refugees seats, the same arrangement presents as insurance: a standing rescue option whose costs are borne elsewhere. From the israeli_citizenry seat it presents as a fair formula imperfectly realized — they pay blood for it and resent being classified as its mere beneficiaries. From the great_power_patrons seat it is order-maintenance, repriced with strategy. Identity-lock dynamics concentrate in zionist_institutions: these bodies are institutionally fused with the claim — their mandates, constitutions, and fundraising identities ARE the parity project — so exit is unthinkable without self-dissolution; if that fusion broke (for instance, if diaspora institutions reconstituted around diaspora pluralism), the claim's administrative backbone would fragment and its persistence would rest on persuasion alone.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive the derivation: jewish_diaspora_communities, israeli_citizenry, and persecuted_jewish_refugees sit near the beneficiary end (low d); palestinian_national_community, declared victim and seated as payer with trapped exit, sits near the full-target end (high d) — trapped exit pushes it further toward full target than a mobile payer would sit. One directionality override is authored for the institutional power atom (d = 0.18): three institutional seats (israeli_citizenry, zionist_institutions, great_power_patrons) all sit near the beneficiary end, but the automatic derivation cannot see this from their mixed signatures — the citizenry pays heavy visible costs (conscription, war dead) that could read as targeting, the patrons hold no beneficiary listing, and the institutions appear as neutral administrators. Structurally all three accrue the arrangement's principal yields, so the override corrects the whole institutional band downward together; the organized band needs no override because role declarations already split the diaspora (low d) from the palestinian community (high d) at the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — mass statelessness and recurrent persecution of a dispersed people in the nation-state era — is genuinely contested in status: the danger is corroborated from outside the beneficiary set (Holocaust historiography, the diplomatic record, contemporary security data), while the adequacy of the sovereignty remedy is precisely what the sibling readings dispute. Because status is 'contested' rather than 'dead', the R5 mismatch consumer finds no dead-problem-plus-world-rearranges flag: the arrangement is not a zombie mandate. The classification discipline cuts both ways here. Claiming rope prevents the mislabel that would read the whole arrangement as pure extraction — a reading that would erase the refuge function that has concretely rescued persecuted Jews in every crisis window since 1933. Authoring honest nonzero metrics and a real payer seat prevents the opposite mislabel — a costless parity claim — which would erase the displacement history and the non-consent at codification. The mandatrophy question stays open through the partition_feasibility and victim_seat_under_own_lights omegas rather than being settled by fiat in either direction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading (liberal_nationalist_reading) of the kernel jewish_self_determination; which reading governs any given classification of ''the Jewish self-determination claim''?',
    'Corpus-level comparison across the five sibling stories; the unlabeled claim has no defined classification until a reading is specified.',
    'Sibling readings assign different epsilon and victim sets: the settler_colonial reading raises epsilon sharply and centers palestinian victims; the religious_covenant reading shifts grounding to theological title; the diasporist reading removes the refuge warrant and locates the hazard in the state itself; the indigenous_return reading drives epsilon toward zero.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed identity of the constraint within its kernel.').

omega_variable(
    disagreement_location_structural_elements,
    'Where exactly do the sibling readings disagree — on the source of the claim (secular parity vs divine covenant vs indigeneity), on the victim set, or on whether the nation-state frame itself is the harm?',
    'Element-by-element diff across the sibling stories'' axioms, victim declarations, and reference frames.',
    'If the disagreement is located in the frame rather than the claim, no quantity of evidence about persecution or historical attachment resolves it; classification divergence across readings is permanent and must be carried per-story rather than averaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disagreement_location_structural_elements, conceptual, 'Locates the kernel contest in specific structural elements rather than a general dispute.').

omega_variable(
    partition_feasibility,
    'Is territorial partition with mutual recognition achievable, or does the parity formula presuppose a settlement the parties cannot actually reach?',
    'Negotiation-history analysis and convergence testing across successive frameworks (Peel, 181, Oslo parameters): do offers converge toward a stable two-claim settlement?',
    'If partition is infeasible, the principle''s operation perpetuates open-ended asymmetry: epsilon rises, the effective type drifts toward tangled_rope, and the palestinian payer seat hardens toward full-target directionality.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility, empirical, 'Feasibility of the settlement formula on which this reading''s low-to-moderate epsilon depends.').

omega_variable(
    principle_vs_conduct_attribution,
    'Is the measured extraction attributable to the parity principle itself, or to state conduct exceeding it (occupation and settlement expansion beyond the partition premise)?',
    'Counterfactual compliance analysis: model epsilon under strict adherence to the partition-and-mutual-recognition formula versus observed conduct, and attribute the difference.',
    'If conduct exceeds principle, this story''s epsilon overstates the principle''s intrinsic extraction; the state''s conduct warrants a separate constraint story, keeping this reading''s classification clean rather than charging the implementer''s drift to the claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(principle_vs_conduct_attribution, empirical, 'Separates the principle''s intrinsic epsilon from implementer drift.').

omega_variable(
    victim_seat_under_own_lights,
    'Does this reading''s own frame contain a victim seat at all, or only costs-of-settlement borne by a co-equal claimant?',
    'Consent analysis: costs accepted as the price of a symmetric bargain differ structurally from costs imposed without a bargained frame; test whether the palestinian community ever consented to the partition premise at any codifying moment.',
    'If no consent frame ever obtained, the payer seat computes nearer full-target and the arrangement trends tangled_rope despite the reading''s parity intent; if a consent frame is credited, the borne costs remain settlement-price and the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_seat_under_own_lights, conceptual, 'Whether the reading''s ''no victim in principle'' survives contact with consent facts.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 1897, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_libnat_tr_t1897, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1897, 0.08).
narrative_ontology:measurement_basis(jsd_libnat_tr_t1897, observed).
narrative_ontology:measurement(jsd_libnat_tr_t1922, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1922, 0.1).
narrative_ontology:measurement_basis(jsd_libnat_tr_t1922, observed).
narrative_ontology:measurement(jsd_libnat_tr_t1947, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1947, 0.14).
narrative_ontology:measurement_basis(jsd_libnat_tr_t1947, observed).
narrative_ontology:measurement(jsd_libnat_tr_t1967, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1967, 0.19).
narrative_ontology:measurement_basis(jsd_libnat_tr_t1967, observed).
narrative_ontology:measurement(jsd_libnat_tr_t1988, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1988, 0.21).
narrative_ontology:measurement_basis(jsd_libnat_tr_t1988, observed).
narrative_ontology:measurement(jsd_libnat_tr_t1993, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 1993, 0.17).
narrative_ontology:measurement_basis(jsd_libnat_tr_t1993, observed).
narrative_ontology:measurement(jsd_libnat_tr_t2015, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2015, 0.25).
narrative_ontology:measurement_basis(jsd_libnat_tr_t2015, observed).
narrative_ontology:measurement(jsd_libnat_tr_t2026, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 2026, 0.28).
narrative_ontology:measurement_basis(jsd_libnat_tr_t2026, observed).

% Extraction over time
narrative_ontology:measurement(jsd_libnat_be_t1897, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1897, 0.16).
narrative_ontology:measurement_basis(jsd_libnat_be_t1897, observed).
narrative_ontology:measurement(jsd_libnat_be_t1922, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1922, 0.26).
narrative_ontology:measurement_basis(jsd_libnat_be_t1922, observed).
narrative_ontology:measurement(jsd_libnat_be_t1947, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1947, 0.36).
narrative_ontology:measurement_basis(jsd_libnat_be_t1947, observed).
narrative_ontology:measurement(jsd_libnat_be_t1967, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1967, 0.44).
narrative_ontology:measurement_basis(jsd_libnat_be_t1967, observed).
narrative_ontology:measurement(jsd_libnat_be_t1988, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1988, 0.41).
narrative_ontology:measurement_basis(jsd_libnat_be_t1988, observed).
narrative_ontology:measurement(jsd_libnat_be_t1993, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 1993, 0.29).
narrative_ontology:measurement_basis(jsd_libnat_be_t1993, observed).
narrative_ontology:measurement(jsd_libnat_be_t2015, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2015, 0.35).
narrative_ontology:measurement_basis(jsd_libnat_be_t2015, observed).
narrative_ontology:measurement(jsd_libnat_be_t2026, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 2026, 0.32).
narrative_ontology:measurement_basis(jsd_libnat_be_t2026, observed).

% Suppression requirement over time
narrative_ontology:measurement(jsd_libnat_su_t1897, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1897, 0.06).
narrative_ontology:measurement_basis(jsd_libnat_su_t1897, observed).
narrative_ontology:measurement(jsd_libnat_su_t1922, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1922, 0.14).
narrative_ontology:measurement_basis(jsd_libnat_su_t1922, observed).
narrative_ontology:measurement(jsd_libnat_su_t1947, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1947, 0.24).
narrative_ontology:measurement_basis(jsd_libnat_su_t1947, observed).
narrative_ontology:measurement(jsd_libnat_su_t1967, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1967, 0.34).
narrative_ontology:measurement_basis(jsd_libnat_su_t1967, observed).
narrative_ontology:measurement(jsd_libnat_su_t1988, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1988, 0.32).
narrative_ontology:measurement_basis(jsd_libnat_su_t1988, observed).
narrative_ontology:measurement(jsd_libnat_su_t1993, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 1993, 0.27).
narrative_ontology:measurement_basis(jsd_libnat_su_t1993, observed).
narrative_ontology:measurement(jsd_libnat_su_t2015, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2015, 0.38).
narrative_ontology:measurement_basis(jsd_libnat_su_t2015, observed).
narrative_ontology:measurement(jsd_libnat_su_t2026, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 2026, 0.42).
narrative_ontology:measurement_basis(jsd_libnat_su_t2026, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'Jewish self-determination' decomposes into five structurally distinct constraints (one per reading of the kernel), each with its own epsilon, beneficiary/victim structure, and classification. This story is the liberal_nationalist member. The liberal reading is diplomatically upstream: its parity frame supplied the register in which the indigenous_return argument operates internationally (influences edge), while the settler_colonial, religious_covenant, and diasporist readings coexist as rival live positions. Epsilon differs across the family by construction: indigenous_return drives epsilon toward zero, this reading sits low-to-moderate (0.32), settler_colonial sits high with an explicit palestinian victim set, religious_covenant relocates grounding to theological title, and diasporist removes the refuge warrant entirely. No single story may hedge across these; linkage is via network edges only.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jewish_self_determination__liberal_nationalist_reading, institutional, 0.18).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
