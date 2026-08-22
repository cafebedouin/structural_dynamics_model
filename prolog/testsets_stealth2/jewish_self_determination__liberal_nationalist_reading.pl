% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__liberal_nationalist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-20
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
 *   constraint_id: jewish_self_determination__liberal_nationalist_reading
 *   human_readable: Jewish National Self-Determination Claim (Liberal-Nationalist Reading)
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This story authors the liberal-nationalist reading of the Jewish
 *   self-determination claim: that the Jewish people constitute a nation, and
 *   that their claim to collective self-governance carries the same standing
 *   as any other people's. The standing arrangement under contest is the
 *   claim as institutionalized — codified in the 1947 partition resolution
 *   and Israel's Declaration of Independence, maintained by diplomatic
 *   recognition, and realized in a sovereign state whose legitimacy argument
 *   rests on national symmetry under the general self-determination doctrine
 *   rather than on historical title or divine grant. The claim's career since
 *   1945 shows the structure this reading predicts: extraction tracks the
 *   feasibility of partition and mutual recognition, dropping when
 *   recognition was exchanged in 1993 and rising as the territorial
 *   arrangement drifted from the partition frame. The claimed type (rope) is
 *   this reading's structural truth — a coordination device among competing
 *   national claims — while the metrics are authored descriptively; the
 *   divergence the engine computes at the cost-bearing seats is the
 *   measurement this corpus exists to take. Interval t=0..80 maps to
 *   1945..2025.
 *
 * KEY AGENTS:
 *   - jewish_diaspora_seeking_refuge: Primary beneficiary (organized/constrained) — the claim maintains their refuge option and international standing
 *   - jewish_national_institutions: Agenda-setter and collector of the claim's vindication (institutional/identity_locked) — articulates and administers the claim in diplomacy, law, and education
 *   - palestinian_national_movement: Cost-bearing counterparty (organized/trapped) — bears partition's allocation costs while holding the mirror national claim
 *   - partition_territory_residents: Diffuse cost-bearers (powerless/trapped) — displacement, border-drawing, and minority or non-citizen status
 *   - international_recognition_system: Codifier and secondary beneficiary (institutional/constrained) — administered partition and collects doctrinal vindication
 *   - diaspora_pluralist_jews: Excluded voice (organized/identity_locked) — holds that sovereignty is not constitutive of Jewish collective life; outside the conversation this claim organizes
 *   - liberal_nationalist_theorists: Analytical observer (analytical/analytical) — adjudicates the claim against the general principles that legitimate national claims elsewhere
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, 0.55).
domain_priors:suppression_score(jewish_self_determination__liberal_nationalist_reading, 0.55).
domain_priors:theater_ratio(jewish_self_determination__liberal_nationalist_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(jewish_self_determination__liberal_nationalist_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__liberal_nationalist_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__liberal_nationalist_reading, "Jewish National Self-Determination Claim (Liberal-Nationalist Reading)").
narrative_ontology:topic_domain(jewish_self_determination__liberal_nationalist_reading, "political philosophy / nationalism studies / postcolonial theory").

domain_priors:requires_active_enforcement(jewish_self_determination__liberal_nationalist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__liberal_nationalist_reading, '5151adfb-ec1c-4391-8c81-1a484c78758f').
narrative_ontology:cs_kernel_codification('5151adfb-ec1c-4391-8c81-1a484c78758f', formalized).
narrative_ontology:cs_authority_grounding('5151adfb-ec1c-4391-8c81-1a484c78758f', expertise).
narrative_ontology:cs_interpretation_layer_present('5151adfb-ec1c-4391-8c81-1a484c78758f').
narrative_ontology:cs_reading_relation('5151adfb-ec1c-4391-8c81-1a484c78758f', jewish_self_determination__indigenous_return_reading, coexists_with).
narrative_ontology:cs_reading_relation('5151adfb-ec1c-4391-8c81-1a484c78758f', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('5151adfb-ec1c-4391-8c81-1a484c78758f', jewish_self_determination__diasporist_reading, coexists_with).
narrative_ontology:cs_reading_relation('5151adfb-ec1c-4391-8c81-1a484c78758f', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_axiom('5151adfb-ec1c-4391-8c81-1a484c78758f', foundational, jewish_nationhood_carries_equal_self_determination_right).
narrative_ontology:cs_axiom_status(jewish_nationhood_carries_equal_self_determination_right, holdable).
narrative_ontology:cs_axiom_grounding('5151adfb-ec1c-4391-8c81-1a484c78758f', jewish_nationhood_carries_equal_self_determination_right, conventional).
narrative_ontology:cs_axiom('5151adfb-ec1c-4391-8c81-1a484c78758f', secondary, partition_and_mutual_recognition_resolve_competing_claims).
narrative_ontology:cs_axiom_status(partition_and_mutual_recognition_resolve_competing_claims, holdable).
narrative_ontology:cs_axiom_grounding('5151adfb-ec1c-4391-8c81-1a484c78758f', partition_and_mutual_recognition_resolve_competing_claims, instrumental).
narrative_ontology:cs_reference_frame('5151adfb-ec1c-4391-8c81-1a484c78758f', wilsonian_symmetric_self_determination).
narrative_ontology:cs_drift_state('5151adfb-ec1c-4391-8c81-1a484c78758f', contemporary_post_oslo_collapse_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('5151adfb-ec1c-4391-8c81-1a484c78758f', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, jewish_national_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, partition_territory_residents).
narrative_ontology:constraint_beneficiary(jewish_self_determination__liberal_nationalist_reading, international_recognition_system).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement).
narrative_ontology:constraint_victim(jewish_self_determination__liberal_nationalist_reading, partition_territory_residents).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, wilsonian_self_determination_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__liberal_nationalist_reading, two_state_partition_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% World Jewish communities outside Israel, organized through federations, congregations, and advocacy bodies. For them the claim maintains an open door: a state that recognizes them as members entitled to citizenship, and a standing argument that a people without a state remains exposed. What flows to them is the refuge option and international standing; what flows from them is political and financial support for the institutions that hold the claim. Exit would mean renouncing the refuge option — possible for individuals, not for the communities as such.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_diaspora_seeking_refuge, beneficiary,
    organized, generational, constrained, global).

% The Zionist movement's successor bodies — the state, the Jewish Agency, the major diaspora federations — that articulate the claim in diplomacy, law, and education, and administer its realization. They collect the claim's vindication: recognition, sovereignty, and the legitimacy that comes from the symmetry argument. Their organizational identity is constituted by the claim; abandoning it would dissolve what they are. They set the terms on which the claim is argued and defended.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, jewish_national_institutions, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, jewish_national_institutions, beneficiary).

% The organized Palestinian national movement (PLO, PA, and party structures). It holds a mirror claim to the same territory and bears the arrangement's heaviest costs: the partition that recognized Jewish self-determination also allocated land it claims, and the state built on the claim has governed populations it counts as its own. What flows to it is reciprocal recognition of its own nationhood — the 1993 exchange acknowledged it — and what flows from it is the competing claim that makes the symmetry argument necessary. It cannot exit the territorial encounter; its options are negotiation, resistance, or endurance.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, payer,
    organized, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, palestinian_national_movement, beneficiary).

% The people living on the land the claim's realization allocated — Palestinian Arabs displaced in 1948 or living under the state's administration, and the small Jewish populations who left the areas allocated to the Arab state. They bear the transition costs directly: displacement, border-drawing, minority or non-citizen status. On this reading's own terms they are owed citizenship and equality in whichever state governs them; where that has not materialized, they bear the gap. They have no exit from the territory's arrangement.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, partition_territory_residents, payer,
    powerless, generational, trapped, regional).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, partition_territory_residents, beneficiary).

% The UN system and the great-power chancelleries that administer recognition of national claims. They codified the arrangement at its founding moment (the 1947 partition resolution) and collect the vindication of their own doctrine: the case demonstrates that the self-determination framework can adjudicate a two-claimants-one-territory dispute. Derecognition is possible but costly — it would unsettle the doctrine they administer.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, international_recognition_system, agenda_setter,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__liberal_nationalist_reading, international_recognition_system, beneficiary).

% Jewish intellectuals and communities who hold that Jewish collective life flourishes as diaspora pluralism and that sovereignty is not constitutive of Jewish peoplehood. They are largely outside the conversation this claim organizes — Jewish institutional life and diplomatic discourse treat the claim as settled common ground — and their objection, that the claim ties Jewish fate to a state's conflicts, is heard as dissent rather than as a competing frame. Their Jewish identity binds them to the very communal structures whose common ground they dispute.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, diaspora_pluralist_jews, excluded,
    organized, generational, identity_locked, global).

% Political theorists working in the national self-determination tradition who assess whether the claim satisfies the general principles — peoplehood, non-expulsion of others, minority protection — that legitimate national claims elsewhere. They take testimony from every seat, and their assessments form the interpretive layer that adjudicates the claim's consistency with the doctrine it invokes.
narrative_ontology:constraint_stakeholder(jewish_self_determination__liberal_nationalist_reading, liberal_nationalist_theorists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__liberal_nationalist_reading, jewish_national_institutions).
narrative_ontology:fixing_cost_class(jewish_self_determination__liberal_nationalist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Places the Jewish national claim inside the nation-state framework where it becomes legible and negotiable: a stateless people's claim is recognized as the same kind of claim other nations hold, creating a common currency — recognition, partition, treaties — in which the Jewish and Palestinian claims can be transacted instead of fought to exhaustion.
% TRANSFER_FUNCTION: Moves recognition, legitimacy, and — in realization — territory and sovereign authority toward the Jewish national community; moves the costs of territorial partition (displacement, border-drawing, minority status, the refugee question) onto the territory's populations and the competing national movement, held in principle to be reciprocal and answered by mutual recognition.
% ABSENT_VOICES: Diaspora-pluralist Jews (stakeholder diaspora_pluralist_jews) would object that sovereignty is not constitutive of Jewish collective life and that the claim binds Jewish fate to a state's conflicts. Palestinian voices reading the claim's realization as dispossession rather than partition are present only as the counterparty to be recognized, not as co-authors of the frame. Both sit outside the liberal-nationalist conversation in which the claim's symmetry and the partition solution are common ground.
% DISAPPEARANCE_RATIONALE: If the claim vanished overnight: Israeli state legitimacy loses its doctrinal foundation (the Declaration of Independence's argument structure collapses), the two-state paradigm and every partition-based peace framework lose their premise, the diaspora's refuge-and-standing arguments disappear, and the conflict's diplomatic language — recognition, symmetry, mutual acknowledgment — has no framework. The regional order rearranges around raw power or around whichever sibling reading replaces the claim.
% FOUNDING_PROBLEM: The nineteenth-century nationality principle made statelessness existentially dangerous for a people others defined as a nation: with no state to hold citizenship in, Jews were stripped of citizenship (Nuremberg Laws), refused refuge (the Évian Conference of 1938), and murdered at industrial scale. The claim was built to solve: how does a stateless nation acquire the standing, protection, and refuge that in the nation-state system only sovereign self-determination provides?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties by the historical record (citizenship-stripping under the Nuremberg Laws, the Évian Conference's refusal of refuge, Holocaust historiography identifying statelessness as an enabling condition) and by contemporary refuge episodes (Soviet Jewry, Ethiopian aliyah, post-2023 surges in antisemitic violence driving emigration). Diaspora-pluralist scholars dispute the corollary — that sovereignty is the necessary remedy — which is a contest over the solution, not over the founding problem's existence.
narrative_ontology:disappearance_verdict(jewish_self_determination__liberal_nationalist_reading, world_rearranges).
narrative_ontology:founding_problem_status(jewish_self_determination__liberal_nationalist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__liberal_nationalist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jewish_self_determination__liberal_nationalist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__liberal_nationalist_reading, 0.55, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__liberal_nationalist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__liberal_nationalist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__liberal_nationalist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored at 0.55 (moderate) because the standing arrangement's costs fall on the territory's populations and the competing national movement — displacement, governance without citizenship in the occupied territories, the refugee question — which this reading attributes to the failure of partition acceptance rather than to the principle itself; the temporal series makes the reading's own stated dependency explicit, dropping at the 1993 mutual-recognition exchange (0.35) and rising as the partition frame eroded (0.55 at t=80). Suppression 0.55: the arrangement's persistence requires active enforcement — the state's coercive maintenance of the territorial frame — while discursively the rival readings of Jewish collective life coexist and are not suppressed; the suppression_requirement series is authored because this story specifically tracks enforcement-capacity change (establishment war, occupation administration, Oslo-era relaxation, post-2002 ratchet), not merely extraction drift. Theater 0.38: a growing share of the claim's maintenance is performative legitimacy defense (hasbara infrastructure, delegitimization counter-campaigns) rather than substantive diplomacy, but the refuge and recognition functions remain operative, keeping theater below the substitution threshold. Accessibility_collapse 0.45: alternatives — rival readings, binational arrangements — remain live and arguable; the claim forecloses none of them discursively. Resistance 0.65: the claim meets sustained organized resistance from the competing national movement and from delegitimization campaigns. All three tracked metrics share one time grid (t = 0, 3, 22, 30, 48, 57, 68, 80); final values match the base_properties scalars. The claimed type is authored from this reading's structural truth independently of the metrics; no tuning toward a predicted engine verdict was performed. The identity_coordination declaration reflects the claim's genuine function — coordinating membership and recognition claims against evolving criteria — not a cover story; the extraction sits in the realization's costs, which the metrics carry separately.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the beneficiary seats (the diaspora, the national institutions) the arrangement reads as coordination: refuge, recognition, and a common currency in which competing claims can be transacted. From the payer seat (the Palestinian national movement — trapped, bearing partition's allocation costs) the same structure computes as enforced extraction with high effective chi. The territory's residents sit closest to full target: powerless, no exit, bearing displacement and status costs directly. The analytical seat sees the symmetry premise as the crux on which the whole classification turns — which is why it is carried as an omega rather than resolved here.
 *
 * DIRECTIONALITY LOGIC:
 *   The derivation chain reads the declared beneficiaries (jewish_diaspora_seeking_refuge, jewish_national_institutions) to low d — the claim subsidizes them with standing and a refuge option — and the payer roles (palestinian_national_movement, partition_territory_residents) to high d, with trapped exit pushing both toward the full-target end. No directionality overrides are authored: the override mechanism keys on the power atom, and the two organized seats (the diaspora and the Palestinian movement) need different d values that a single atom cannot separate — the role and exit data already differentiate them. The national institutions sit at low d despite being agenda-setters: administering the claim and collecting its vindication is a beneficiary position within this reading's structure, not a capture position.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — stateless-nation exposure in the nation-state system — remains live, so no mandatrophy is declared: the refuge and recognition functions the claim performs are still operative, which is why theater stays below 0.5 even as legitimacy defense grows. The analysis guards against two mislabels. Reading the claim as pure coordination would erase the payer seat's costs, which the metrics refuse to do; reading it as pure extraction would erase the genuine refuge and recognition functions that the 1993 dip in the epsilon series records. If the founding problem were ever dead — universal safety without sovereignty — the claim would drift toward atrophied maintenance, and the R5 mismatch check (status=dead combined with verdict=world_rearranges) would flag it for investigation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This story is one reading of the kernel jewish_self_determination — the liberal_nationalist_reading. Which structural facts would change if a sibling reading were instantiated instead, and where exactly does this reading''s commitment diverge from them?',
    'Compare the sibling story files (indigenous_return, settler_colonial, religious_covenant, diasporist) at their own epsilon, victim structure, and type. The disagreement is located in two elements: the ground of the claim (generic nationhood under the self-determination doctrine vs. indigeneity vs. divine covenant vs. anti-nationalist pluralism) and the structural status of the Palestinian claim (symmetric counterparty vs. dispossessed victim vs. outside the frame).',
    'Under the settler_colonial structure, epsilon moves high and displaced Palestinians are declared victims; under the diasporist structure, the beneficiary set dissolves and the arrangement reads as imposed on Jewish collective life. This file''s rope claim, moderate epsilon, and no-victims structure hold only within this reading''s commitments.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer structure: this constraint is the liberal-nationalist reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    partition_feasibility,
    'Does territorial partition with mutual recognition — the resolution mechanism this reading assumes — remain feasible, or has the post-1967 territorial reality made the reading''s coordination story inoperative?',
    'Negotiation-track evidence: whether a two-state agreement with mutual recognition remains achievable under observable conditions (settlement footprint, leadership positions, third-party guarantees, demographic trajectories).',
    'If partition is infeasible, the reading''s coordination function fails while the arrangement persists by enforcement — classification drifts from rope toward enforced extraction, and epsilon rises toward the payer seat''s experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(partition_feasibility, empirical, 'Feasibility of the partition mechanism the reading''s coordination structure depends on.').

omega_variable(
    symmetry_premise_status,
    'Is the claim''s core premise — that the Jewish and Palestinian national claims are structurally symmetric — sustainable, or does the asymmetry between a sovereign state and a stateless people make the symmetry formal rather than real?',
    'Compare the parties'' standing under the arrangement: recognition status, territorial control, coercive capacity, and whether the arrangement''s costs fall reciprocally in fact rather than only in principle.',
    'If the asymmetry is constitutive rather than contingent, this reading''s epsilon understates the extraction the payer seat experiences and the reading converges structurally toward the settler_colonial sibling''s constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(symmetry_premise_status, conceptual, 'Whether the symmetry premise is real or formal.').

omega_variable(
    jewish_nationhood_sociological_basis,
    'Do the Jewish people constitute a nation in the sociological sense the claim requires — a self-conceived people with collective institutions and a self-governance aspiration — or is Jewish peoplehood a religious-civilizational identity that the national frame distorts?',
    'Sociological and survey evidence on Jewish self-identification across diaspora communities; the institutional record (Hebrew revival, national institutions, sustained collective political action).',
    'If nationhood fails as an empirical description, the claim''s premise collapses and this reading dissolves into the religious_covenant or diasporist siblings; the rope classification and its beneficiary structure presuppose nationhood.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(jewish_nationhood_sociological_basis, empirical, 'Empirical status of the nationhood premise.').

omega_variable(
    authority_grounding_framing,
    'Is expertise — adjudication by political theory and international law — the right authority grounding for this reading, or does the claim''s operative authority actually rest on lineage (continuity of the Declaration of Independence''s argument) or on the self-enforcing fact of the state''s existence?',
    'Trace what actually adjudicates disputes over the claim''s scope in practice: doctrinal argument and legal-theoretical interpretation (expertise), founding-text continuity (lineage), or the settled fact of statehood (self_enforcing).',
    'Under lineage grounding the reading moves toward the indigenous_return sibling''s authority structure; under self_enforcing grounding the interpretive layer thins and drift surfaces directly rather than being absorbed by interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(authority_grounding_framing, conceptual, 'Framing under-determination of the reading''s authority structure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__liberal_nationalist_reading, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jsd_liberal_nationalist_tr_t0, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jsd_liberal_nationalist_tr_t3, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 3, 0.15).
narrative_ontology:measurement(jsd_liberal_nationalist_tr_t22, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 22, 0.18).
narrative_ontology:measurement(jsd_liberal_nationalist_tr_t30, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 30, 0.3).
narrative_ontology:measurement(jsd_liberal_nationalist_tr_t48, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 48, 0.2).
narrative_ontology:measurement(jsd_liberal_nationalist_tr_t57, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 57, 0.28).
narrative_ontology:measurement(jsd_liberal_nationalist_tr_t68, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 68, 0.32).
narrative_ontology:measurement(jsd_liberal_nationalist_tr_t80, jewish_self_determination__liberal_nationalist_reading, theater_ratio, 80, 0.38).

% Extraction over time
narrative_ontology:measurement(jsd_liberal_nationalist_be_t0, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(jsd_liberal_nationalist_be_t3, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 3, 0.45).
narrative_ontology:measurement(jsd_liberal_nationalist_be_t22, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 22, 0.5).
narrative_ontology:measurement(jsd_liberal_nationalist_be_t30, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 30, 0.52).
narrative_ontology:measurement(jsd_liberal_nationalist_be_t48, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 48, 0.35).
narrative_ontology:measurement(jsd_liberal_nationalist_be_t57, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 57, 0.48).
narrative_ontology:measurement(jsd_liberal_nationalist_be_t68, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 68, 0.52).
narrative_ontology:measurement(jsd_liberal_nationalist_be_t80, jewish_self_determination__liberal_nationalist_reading, base_extractiveness, 80, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(jsd_liberal_nationalist_su_t0, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(jsd_liberal_nationalist_su_t3, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 3, 0.5).
narrative_ontology:measurement(jsd_liberal_nationalist_su_t22, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 22, 0.55).
narrative_ontology:measurement(jsd_liberal_nationalist_su_t30, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 30, 0.5).
narrative_ontology:measurement(jsd_liberal_nationalist_su_t48, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 48, 0.35).
narrative_ontology:measurement(jsd_liberal_nationalist_su_t57, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 57, 0.5).
narrative_ontology:measurement(jsd_liberal_nationalist_su_t68, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 68, 0.52).
narrative_ontology:measurement(jsd_liberal_nationalist_su_t80, jewish_self_determination__liberal_nationalist_reading, suppression_requirement, 80, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__liberal_nationalist_reading, identity_coordination).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__indigenous_return_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__liberal_nationalist_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Jewish claim to self-determination' decomposes into at least five structurally distinct constraints — the kernel's readings. This file instantiates the liberal-nationalist reading: legitimacy grounded in generic nationhood under the international self-determination doctrine, resolution by partition and mutual recognition, no victims in principle, moderate epsilon. The settler_colonial sibling authors high epsilon with displaced Palestinians as victims; the indigenous_return sibling authors low epsilon grounded in decolonization; the religious_covenant sibling grounds the claim theologically and independent of secular frameworks; the diasporist sibling denies the sovereignty premise outright. Epsilon differs across the family because each reading assesses a different constraint, not because one constraint is measured different ways — the decomposition follows the epsilon-invariance principle.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
