% ============================================================================
% CONSTRAINT STORY: jewish_self_determination__indigenous_return_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jewish_self_determination__indigenous_return_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: jewish_self_determination__indigenous_return_reading
 *   human_readable: Indigenous Return Reading of Jewish Self-Determination
 *   domain: political philosophy / nationalism studies / postcolonial theory
 *
 * SUMMARY:
 *   This story instantiates one reading of the jewish_self_determination
 *   kernel: the claim that Jewish people possess unbroken indigenous
 *   connection to the land, such that Zionism is properly understood as
 *   decolonization (the return of a dispossessed indigenous people) rather
 *   than colonization (the settlement of an external population over a native
 *   one). This reading is analytically distinct from the
 *   liberal_nationalist_reading (which grounds the claim in ordinary national
 *   self-determination without an indigeneity/colonization framework), the
 *   settler_colonial_reading (which inverts the colonization verdict
 *   entirely), the religious_covenant_reading (which grounds the claim in
 *   divine mandate rather than historical-anthropological indigeneity), and
 *   the diasporist_reading (which rejects territorial sovereignty as the
 *   vehicle for Jewish self-determination altogether). Per the ε-invariance
 *   principle, these are five separate constraints, not five measurements of
 *   one constraint — each carries a different ε, different beneficiary/victim
 *   structure, and different classification. This file addresses ONLY the
 *   indigenous_return_reading.
 *
 * KEY AGENTS:
 *   - jewish_claimants_to_ancestral_land: primary beneficiary of the classification (organized/institutional) — the reading vindicates sovereignty claims
 *   - israeli_state_legitimacy_project: institutional beneficiary — the reading supplies legitimating vocabulary against international decolonization norms
 *   - palestinian_national_movement: structurally subordinated by the reading's classification act, though the reading itself declares no victim
 *   - international_legal_and_historical_scholarship: analytical observer — contested terrain of indigeneity criteria
 *   - diaspora_jewish_communities: differently positioned — some benefit from the legitimation, others (per the diasporist sibling) view it as a liability
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, 0.68).
domain_priors:suppression_score(jewish_self_determination__indigenous_return_reading, 0.55).
domain_priors:theater_ratio(jewish_self_determination__indigenous_return_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(jewish_self_determination__indigenous_return_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jewish_self_determination__indigenous_return_reading, rope).
narrative_ontology:human_readable(jewish_self_determination__indigenous_return_reading, "Indigenous Return Reading of Jewish Self-Determination").
narrative_ontology:topic_domain(jewish_self_determination__indigenous_return_reading, "political philosophy / nationalism studies / postcolonial theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jewish_self_determination__indigenous_return_reading, 'e665bf47-bae2-4459-bfec-3ccf0b9c13c1').
narrative_ontology:cs_kernel_codification('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', distributed).
narrative_ontology:cs_authority_grounding('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', distributed).
narrative_ontology:cs_reading_relation('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', jewish_self_determination__liberal_nationalist_reading, influences).
narrative_ontology:cs_reading_relation('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', jewish_self_determination__settler_colonial_reading, forecloses).
narrative_ontology:cs_reading_relation('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', jewish_self_determination__religious_covenant_reading, coexists_with).
narrative_ontology:cs_reading_relation('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', jewish_self_determination__diasporist_reading, influences).
narrative_ontology:cs_axiom('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', foundational, unbroken_ancestral_continuity_confers_present_indigeneity).
narrative_ontology:cs_axiom_status(unbroken_ancestral_continuity_confers_present_indigeneity, holdable).
narrative_ontology:cs_axiom_grounding('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', unbroken_ancestral_continuity_confers_present_indigeneity, empirically_contingent).
narrative_ontology:cs_axiom('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', foundational, return_after_dispersion_is_decolonization_not_settlement).
narrative_ontology:cs_axiom_status(return_after_dispersion_is_decolonization_not_settlement, holdable).
narrative_ontology:cs_axiom_grounding('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', return_after_dispersion_is_decolonization_not_settlement, conventional).
narrative_ontology:cs_reference_frame('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', ancient_jewish_territorial_presence).
narrative_ontology:cs_drift_state('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', post_1967_decolonization_era, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('e665bf47-bae2-4459-bfec-3ccf0b9c13c1', '').
narrative_ontology:cs_kernel_id(jewish_self_determination__indigenous_return_reading, jewish_self_determination).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, israeli_state_legitimacy_project).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, unbroken_indigenous_continuity_doctrine).
narrative_ontology:constraint_vindicates(jewish_self_determination__indigenous_return_reading, decolonization_not_colonization_framing).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the historical and religious memory of continuous connection to the land across millennia of dispersion. The indigenous_return_reading validates this memory as a legally and politically cognizable indigeneity claim rather than mere historical sentiment, converting ancestral narrative into standing within international decolonization frameworks. Exit from this framing is difficult because it is fused with communal identity and historical trauma narratives built over generations.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, jewish_claimants_to_ancestral_land, beneficiary,
    organized, civilizational, identity_locked, global).

% Deploys the indigenous_return_reading in diplomatic, legal, and educational contexts to counter accusations of settler-colonialism in international forums. Actively promotes and funds scholarship, advocacy, and public diplomacy built on this framing. Cannot easily abandon the framing without ceding significant ground in an ongoing legitimacy contest, but is not fully trapped — could shift emphasis toward the liberal_nationalist_reading, which requires no indigeneity claim at all.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, israeli_state_legitimacy_project, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, israeli_state_legitimacy_project, agenda_setter).

% Maintains its own continuous-presence and indigeneity claims to the same land, which this reading structurally subordinates by classifying Palestinian presence as later arrival or as a co-indigenous claim of lesser priority. Has no seat in the process by which the indigenous_return_reading is adjudicated or applied in international forums; can contest it rhetorically and legally but cannot exit the territorial dispute the classification bears on.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, palestinian_national_movement, excluded,
    organized, generational, trapped, regional).

% Evaluates competing indigeneity and colonization claims against comparative historical and legal criteria developed largely in other contexts (settler colonies in the Americas, Australia, southern Africa). Notes that the criteria for 'indigenous' status were developed to describe dispossession of a still-present native population by an incoming colonial power, and that applying them to a case involving return after millennia of dispersion strains the concept's ordinary usage; remains divided on whether the reading is a defensible extension or a category error.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, international_legal_and_historical_scholarship, observer,
    analytical, civilizational, analytical, global).

% Some communities embrace the indigenous_return_reading as validating and dignifying; others (aligned with the sibling diasporist_reading) view territorial-sovereignty framings as a liability that ties diaspora safety and standing to a distant state's conduct. Not directly party to how the reading is adjudicated internationally, though invoked rhetorically by all sides.
narrative_ontology:constraint_stakeholder(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities, beneficiary,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(jewish_self_determination__indigenous_return_reading, diaspora_jewish_communities, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jewish_self_determination__indigenous_return_reading, israeli_state_legitimacy_project).
narrative_ontology:fixing_cost_class(jewish_self_determination__indigenous_return_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a shared vocabulary and legal-historical warrant that allows Jewish claimants and the Israeli state to coordinate a unified public case for territorial legitimacy, framed in terms the international community already treats as normatively weighty (decolonization and indigenous rights) rather than requiring a separate justificatory framework.
% TRANSFER_FUNCTION: Moves legitimacy and standing from the framework's application: it transfers rhetorical and legal advantage toward Jewish claimants in international forums where decolonization language carries force, and correspondingly reduces the standing of competing claims (Palestinian) that would otherwise occupy the same normative category.
% ABSENT_VOICES: Palestinian historians, legal scholars, and the Palestinian national movement itself are the primary parties who would contest this reading's classification of their presence as later or subordinate; they are largely absent from the venues (advocacy literature, some legal briefs, educational curricula) where the reading is most actively deployed, though they are highly present in the broader academic and diplomatic contest over it.
% DISAPPEARANCE_RATIONALE: Proponents would say the underlying historical facts of Jewish origin in the land do not depend on the reading's political deployment and would persist regardless — the world would not rearrange because the facts are prior to the framing. Critics would say the political and legal work the reading currently performs (converting sovereignty legitimacy into a decolonization-framework victory) would need to be replaced by some other legitimating vocabulary, and that international diplomatic positioning would shift measurably if the framing were withdrawn or lost persuasive force — hence the dispute is itself irreducible to a single verdict.
% FOUNDING_PROBLEM: As anti-colonial and decolonization norms became increasingly central to international legitimacy after the mid-20th century, Zionism faced a legitimacy challenge: a movement historically framed by its founders substantially in terms of national self-determination and, for some, religious return, needed a vocabulary that could withstand classification as colonial settlement once that classification became reputationally costly.
% FOUNDING_PROBLEM_CORROBORATION: Historians of Zionism (including some sympathetic to Israeli statehood) and international relations scholars outside both the Israeli state apparatus and Palestinian advocacy organizations attest that the indigeneity/decolonization vocabulary intensified specifically in response to the growing international salience of decolonization and settler-colonial studies frameworks from the 1960s onward, rather than being the primary self-description used in early Zionist political thought, which more often invoked national self-determination or religious-historical return without the specific indigenous/colonizer binary now centrally deployed.
narrative_ontology:disappearance_verdict(jewish_self_determination__indigenous_return_reading, contested).
narrative_ontology:founding_problem_status(jewish_self_determination__indigenous_return_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jewish_self_determination__indigenous_return_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jewish_self_determination__indigenous_return_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jewish_self_determination__indigenous_return_reading, 0.68, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jewish_self_determination__indigenous_return_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jewish_self_determination__indigenous_return_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jewish_self_determination__indigenous_return_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is rope: a reading that, if accepted uncontested, would coordinate a claim of historical continuity into a settled allocation of legitimacy with minimal enforcement overhead — the mountain-like ideal case described in the kernel context. But the authored metrics reflect what is descriptively true of the reading's actual operation: extractiveness is substantial (0.68) because the classification act does structural work beyond reporting a historical fact — it is deployed to subordinate a rival claim (Palestinian presence) without naming that subordination as a victim relationship. Resistance is high (0.78) because the reading is fiercely contested by historians, international bodies, and the rival settler_colonial_reading; a genuine mountain would meet negligible resistance. Accessibility collapse is moderate-low (0.4) because alternative framings (the four sibling readings) remain fully live and widely held — the indigeneity framing has not collapsed the interpretive field the way an actual natural-law finding would. Theater ratio has risen over the interval (0.20 to 0.42) as the indigeneity vocabulary has been increasingly deployed in diplomatic and legal advocacy contexts disconnected from the underlying historiographical debate — a proxy-goal substitution pattern (advocacy utility replacing historiographical rigor as the operative function).
 *
 * PERSPECTIVAL GAP:
 *   From the seat of Jewish claimants and the Israeli state legitimacy project, this reading operates as a settled historical premise doing legitimate coordinating work — establishing standing in a decolonization framework that international norms otherwise privilege. From the seat of the Palestinian national movement and much of comparative-indigeneity scholarship, the same reading operates as an extractive reclassification: it takes a genuinely contested co-indigeneity situation and resolves it unilaterally in one party's favor, borrowing the moral authority of the decolonization framework without meeting its usual evidentiary standard (which typically requires demonstrating dispossession of the CURRENT native population by an incoming colonial power, not restoration of an ancient population's own prior presence). This divergence is exactly what the engine is built to register — the claim and the metrics are authored independently and are not reconciled here.
 *
 * DIRECTIONALITY LOGIC:
 *   Jewish claimants to ancestral land and the Israeli state legitimacy project are declared beneficiaries: the reading provides them low-d (near-beneficiary) positioning because it directly subsidizes their legitimacy claim in international discourse. No victim group is declared in base_properties, consistent with the reading's own internal logic (Palestinian presence is reframed as later arrival or subordinate co-indigenous claim, not as a group the reading extracts from). This is itself the analytically significant fact: the reading's structure is built to avoid generating a victim category, even though the omega variables above document that this omission is contestable rather than self-evidently correct. The palestinian_national_movement stakeholder is authored with role 'excluded' rather than 'payer' to reflect the reading's own declared structure — they would object to the classification but are not named as bearing extraction within this reading's terms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem this reading answers — establishing a non-colonial legitimating narrative for Jewish territorial sovereignty against the rising force of international decolonization norms in the mid-20th century — remains structurally live: decolonization vocabulary continues to carry significant normative weight in international forums, and the reading's function (converting a contested sovereignty dispute into a resolved indigeneity finding) continues to do the same legitimating work it always did. This is not mandatrophy in the classic sense (a mandate outliving its function) — the function has not disappeared. What the classification captures instead is a rope-to-contested-classification-act structure: coordination for one party's claim, purchased partly through the suppression of an equally plausible rival framing, sustained by ongoing political utility rather than converging historiographical consensus.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indigeneity_binary_vs_contested,
    'Is indigenous status a binary historical fact (in which case the reading approaches a mountain — an irreducible fact of origin that settles the classification question) or is it an inherently contested classification requiring adjudication between competing claimants (in which case the reading functions as a rope coordinating a contested allocation, with real distributive stakes)?',
    'There is no neutral adjudicator for indigeneity claims in contexts of competing continuous presence; international law bodies, historians, and archaeologists disagree on operational criteria (continuous residence vs. ancestral origin vs. political self-identification). Resolution would require an accepted cross-cultural standard for indigeneity that does not itself favor one claimant''s framework.',
    'If binary and accepted, this reading approaches mountain-like status with negligible epsilon (the fact of origin, once established, need not be defended). If contested, epsilon rises sharply because the classification act itself becomes the site of extraction — determining who counts as indigenous determines whose claim is subordinated, which is exactly the structural work this reading''s proponents need it to do.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(indigeneity_binary_vs_contested, conceptual, 'Whether indigeneity functions as settled historical fact or as a contested classification doing political work.').

omega_variable(
    palestinian_coindigeneity_status,
    'Does this reading''s treatment of Palestinian presence as ''later arrival'' or ''subordinate co-indigenous claim'' accurately represent the historical record, or does it require selective periodization to sustain?',
    'Comparative historical and archaeological analysis of continuous population presence, weighed against the reading''s own admission that both peoples maintain continuity claims of some form.',
    'If the subordination of Palestinian claims requires selective periodization, the reading functions less as an indigeneity finding and more as a priority-ranking device — which shifts the classification toward tangled_rope (coordination function for Jewish self-determination bundled with asymmetric subordination of a rival claim) rather than a clean rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(palestinian_coindigeneity_status, empirical, 'Whether Palestinian co-indigeneity is genuinely subordinate or asymmetrically minimized to sustain the reading.').

omega_variable(
    cs_framing_kernel_vs_dispute,
    'Is the kernel here best modeled as a single stabilized commitment (the historical fact of Jewish origin in the land) with multiple readings layered on top, or is the deeper kernel actually ''who possesses legitimate sovereignty claims in contested territory'' — a distinct and prior question that the indigeneity framing answers only by assumption?',
    'Compare which framing better predicts where actual political and legal contestation occurs: disputes center overwhelmingly on sovereignty and political arrangements, not on the historical fact of ancient Jewish presence (which is comparatively uncontested among historians). This suggests the ''indigeneity'' framing is doing legitimation work for a sovereignty conclusion rather than reporting a settled historical premise.',
    'If the deeper kernel is sovereignty rather than indigeneity, this reading''s decolonization/colonization vocabulary is a rhetorical transposition onto a dispute that indigeneity status alone cannot resolve, and its epsilon should be assessed against the sovereignty dispute''s contestedness, not against the comparatively settled historical-origin question.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_kernel_vs_dispute, conceptual, 'Alternative framing: kernel as sovereignty-legitimacy dispute rather than indigeneity-fact dispute, and how that changes classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jewish_self_determination__indigenous_return_reading, 1897, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jewi_tr_t1897, jewish_self_determination__indigenous_return_reading, theater_ratio, 1897, 0.2).
narrative_ontology:measurement(jewi_tr_t1948, jewish_self_determination__indigenous_return_reading, theater_ratio, 1948, 0.28).
narrative_ontology:measurement(jewi_tr_t1967, jewish_self_determination__indigenous_return_reading, theater_ratio, 1967, 0.32).
narrative_ontology:measurement(jewi_tr_t1993, jewish_self_determination__indigenous_return_reading, theater_ratio, 1993, 0.35).
narrative_ontology:measurement(jewi_tr_t2005, jewish_self_determination__indigenous_return_reading, theater_ratio, 2005, 0.38).
narrative_ontology:measurement(jewi_tr_t2024, jewish_self_determination__indigenous_return_reading, theater_ratio, 2024, 0.42).

% Extraction over time
narrative_ontology:measurement(jewi_be_t1897, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1897, 0.3).
narrative_ontology:measurement(jewi_be_t1948, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1948, 0.5).
narrative_ontology:measurement(jewi_be_t1967, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1967, 0.58).
narrative_ontology:measurement(jewi_be_t1993, jewish_self_determination__indigenous_return_reading, base_extractiveness, 1993, 0.6).
narrative_ontology:measurement(jewi_be_t2005, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2005, 0.63).
narrative_ontology:measurement(jewi_be_t2024, jewish_self_determination__indigenous_return_reading, base_extractiveness, 2024, 0.68).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(jewish_self_determination__indigenous_return_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jewish_self_determination__indigenous_return_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jewish_self_determination__indigenous_return_reading, 0.08).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__liberal_nationalist_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__settler_colonial_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__religious_covenant_reading).
narrative_ontology:affects_constraint(jewish_self_determination__indigenous_return_reading, jewish_self_determination__diasporist_reading).

% DUAL FORMULATION NOTE:
% This story is one of five sibling readings of the jewish_self_determination kernel, each authored as a separate ε-invariant constraint per the decomposition principle. indigenous_return_reading and settler_colonial_reading are the most structurally opposed pair (their core premises about the colonization/decolonization verdict directly conflict); religious_covenant_reading grounds the claim in a wholly different (theological) warrant that does not depend on indigeneity findings at all; liberal_nationalist_reading is the least contested sibling, since it does not require an indigeneity or colonization verdict to ground the self-determination claim; diasporist_reading rejects the territorial-sovereignty premise all five other readings share. See cs_structure.reading_relations for the typed edges.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
