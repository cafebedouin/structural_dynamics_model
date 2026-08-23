% ============================================================================
% CONSTRAINT STORY: jati_practice_norm__orthodox_textual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jati_practice_norm__orthodox_textual_reading, []).

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
 *   constraint_id: jati_practice_norm__orthodox_textual_reading
 *   human_readable: Orthodox Scriptural Varna Fixity: Jati Boundaries as Divine Ordination with Pollution Sanction
 *   domain: social anthropology/religious studies/political economy
 *
 * SUMMARY:
 *   Within the jati_practice_norm kernel, this story instantiates the
 *   orthodox textual reading: the constraint under which jati boundaries are
 *   held to derive from a fixed scriptural varna framework, with deviation
 *   from assigned station treated as ritual pollution demanding sanction. The
 *   referent of every metric is the standing hereditary hierarchy as this
 *   reading constitutes it — boundaries scripturally fixed, mobility
 *   categorically blocked, purity actively policed — never a reformed
 *   alternative. On that referent the machinery is a snare: the coordination
 *   story (a divinely ordained division of labor ordering agrarian society)
 *   is real but subordinate, while the operative function transfers
 *   hereditary labor, service, and deference from pollution-assigned and
 *   service jatis to landed upper-varna households and the scriptural estate,
 *   with persistence depending on enforcement and on exits (conversion,
 *   flight, refusal) being doctrinally unthinkable and materially ruinous.
 *   Per the epsilon-invariance principle this is one of three constraint
 *   stories decomposing the colloquial label of the caste system: the
 *   localized_practice_reading file models boundaries as renegotiable
 *   coordination norms, and the colonial_census_reading file models
 *   externally administered reification; each carries its own epsilon,
 *   beneficiaries, and classification, linked through
 *   network.affects_constraints.
 *
 * KEY AGENTS:
 *   - brahmin_scriptural_authority: Agenda-setting beneficiary (institutional/arbitrage) — composes, transmits, and adjudicates the fixing texts; collects dues and legitimacy rents
 *   - landed_upper_varna_patrons: Primary material beneficiary (powerful/mobile) — receives hereditary labor, service, and produce shares
 *   - dalit_pollution_assigned_jatis: Primary target (powerless/identity_locked) — bears pollution-assigned occupations, segregation, and blocked mobility
 *   - hereditary_service_jatis: Secondary target (powerless/constrained) — bears fixed service obligations while holding graded relative status
 *   - bhakti_anticaste_movements: Excluded dissent (organized/constrained) — voices equality claims outside canonical adjudication
 *   - academic_caste_scholarship: Analytical observer (analytical/analytical) — documents the textual-practice gap and sanction history
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, 0.82).
domain_priors:suppression_score(jati_practice_norm__orthodox_textual_reading, 0.62).
domain_priors:theater_ratio(jati_practice_norm__orthodox_textual_reading, 0.48).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, theater_ratio, 0.48).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(jati_practice_norm__orthodox_textual_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jati_practice_norm__orthodox_textual_reading, snare).
narrative_ontology:human_readable(jati_practice_norm__orthodox_textual_reading, "Orthodox Scriptural Varna Fixity: Jati Boundaries as Divine Ordination with Pollution Sanction").
narrative_ontology:topic_domain(jati_practice_norm__orthodox_textual_reading, "social anthropology/religious studies/political economy").

domain_priors:requires_active_enforcement(jati_practice_norm__orthodox_textual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jati_practice_norm__orthodox_textual_reading, '571eee3b-bdaf-4ba5-9510-066da44ea0bd').
narrative_ontology:cs_kernel_codification('571eee3b-bdaf-4ba5-9510-066da44ea0bd', fixed_text).
narrative_ontology:cs_authority_grounding('571eee3b-bdaf-4ba5-9510-066da44ea0bd', lineage).
narrative_ontology:cs_interpretation_layer_present('571eee3b-bdaf-4ba5-9510-066da44ea0bd').
narrative_ontology:cs_reading_relation('571eee3b-bdaf-4ba5-9510-066da44ea0bd', jati_practice_norm__localized_practice_reading, forecloses).
narrative_ontology:cs_reading_relation('571eee3b-bdaf-4ba5-9510-066da44ea0bd', jati_practice_norm__colonial_census_reading, influences).
narrative_ontology:cs_axiom('571eee3b-bdaf-4ba5-9510-066da44ea0bd', foundational, varna_framework_scripturally_fixed).
narrative_ontology:cs_axiom_status(varna_framework_scripturally_fixed, holdable).
narrative_ontology:cs_axiom_grounding('571eee3b-bdaf-4ba5-9510-066da44ea0bd', varna_framework_scripturally_fixed, theological).
narrative_ontology:cs_axiom('571eee3b-bdaf-4ba5-9510-066da44ea0bd', foundational, deviation_is_ritual_pollution).
narrative_ontology:cs_axiom_status(deviation_is_ritual_pollution, holdable).
narrative_ontology:cs_axiom_grounding('571eee3b-bdaf-4ba5-9510-066da44ea0bd', deviation_is_ritual_pollution, theological).
narrative_ontology:cs_reference_frame('571eee3b-bdaf-4ba5-9510-066da44ea0bd', immutable_scriptural_varna_order).
narrative_ontology:cs_drift_state('571eee3b-bdaf-4ba5-9510-066da44ea0bd', contemporary_postconstitutional_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('571eee3b-bdaf-4ba5-9510-066da44ea0bd', '').
narrative_ontology:cs_kernel_id(jati_practice_norm__orthodox_textual_reading, jati_practice_norm).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, brahmin_scriptural_authority).
narrative_ontology:constraint_beneficiary(jati_practice_norm__orthodox_textual_reading, landed_upper_varna_patrons).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, dalit_pollution_assigned_jatis).
narrative_ontology:constraint_victim(jati_practice_norm__orthodox_textual_reading, hereditary_service_jatis).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, varna_karma_desert_doctrine).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, purity_pollution_cosmology).
narrative_ontology:constraint_vindicates(jati_practice_norm__orthodox_textual_reading, dharmashastra_infallibility).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Composes, transmits, and adjudicates the dharma texts that fix the varna framework; defines purity standards, decides which deviations require sanction, and collects ceremonial dues, first-fruits, and service entitlements justified by the framework. Its adjudicating monopoly depends on the framework remaining fixed: revision would dissolve the monopoly. Exit is structurally irrelevant — it can reinterpret texts, absorb rival practices, and shift patronage while keeping the categorical structure intact.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, brahmin_scriptural_authority, agenda_setter,
    institutional, civilizational, arbitrage, continental).
narrative_ontology:stakeholder_secondary_role(jati_practice_norm__orthodox_textual_reading, brahmin_scriptural_authority, beneficiary).

% Landholding households of the upper varnas who receive customary labor, domestic service, and produce shares from dependent jatis, backed by pollution sanctions and patronage credit dependency. When market wages undercut customary service they can shift to hired labor; when customary service is cheaper they invoke hereditary obligation — the constraint subsidizes whichever arrangement currently favors them, and their land wealth gives them options the dependent jatis lack.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, landed_upper_varna_patrons, beneficiary,
    powerful, generational, mobile, regional).

% Communities assigned the occupations the framework marks as polluting — handling corpses, leather, and waste — and segregated residentially, barred from wells, temples, and schooling. Labor obligations are hereditary and compensated barely above subsistence. Exit runs through conversion or flight, but the pollution doctrine teaches that their station reflects their own karmic deserts, and stigma has historically followed them across religious and geographic boundaries; leaving means forfeiting the only community, occupation, credit access, and marriage network available to them.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, dalit_pollution_assigned_jatis, payer,
    powerless, biographical, identity_locked, regional).

% Barber, washerman, smith, and similar jatis bound to patron households by hereditary service exchange: fixed obligations, fixed payments, negligible bargaining power. They rank above the pollution-assigned jatis and receive reciprocal ritual recognition, which gives them a stake in the order's legitimacy even as it caps their mobility; moving village or trade means abandoning an established client base in a system that will rank them wherever they arrive.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, hereditary_service_jatis, payer,
    powerless, biographical, constrained, regional).

% Vernacular devotional and anti-caste currents teaching spiritual equality and refusing priestly mediation — they would replace the framework's authority basis entirely. Their texts circulate outside the canonical adjudication the scriptural estate controls; their adherents remain embedded in the social order whose boundaries they dispute, exposed to its sanctions while unable to force their claims into the adjudicating channel.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, bhakti_anticaste_movements, excluded,
    organized, generational, constrained, regional).

% Historians and anthropologists documenting the gap between textual templates and practiced boundaries, tracing how sanctions operated across regions and periods, and feeding reform jurisprudence and movement strategy. They hold no position inside the arrangement and bear none of its costs.
narrative_ontology:constraint_stakeholder(jati_practice_norm__orthodox_textual_reading, academic_caste_scholarship, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jati_practice_norm__orthodox_textual_reading, landed_upper_varna_patrons).
narrative_ontology:fixing_cost_class(jati_practice_norm__orthodox_textual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Assigns every community a fixed occupation, marriage pool, and ritual rank in one ranked scheme, so that inter-community service exchange, marriage, and civic contact follow settled rules without renegotiation — who may approach whom, who cooks for whom, who handles what.
% TRANSFER_FUNCTION: Moves hereditary labor, domestic service, produce shares, and ceremonial dues from the pollution-assigned and service jatis upward to landed upper-varna households and the scriptural estate; moves ritual legitimation and status recognition downward.
% ABSENT_VOICES: Those assigned polluting occupations had no seat in the textual conversation that defined them: the dharma corpus was composed, transmitted, and adjudicated by the estate the framework provisioned. Vernacular devotional and anti-caste currents voiced equality claims for centuries, but their objections circulated in languages and genres the authority structure did not recognize as binding, so unanimity in the canonical record reflects exclusion of dissenting seats, not consent.
% DISAPPEARANCE_RATIONALE: If the fixity-and-pollution regime vanished overnight, village labor arrangements would reprice as hereditary obligation became negotiable wage relation, marriage pools would widen beyond jati endogamy, residential segregation would lose its sanction, and the scriptural estate would lose its adjudicating monopoly and its dues stream — the agrarian and ritual economy would reorganize around contract and choice.
% FOUNDING_PROBLEM: Ordering a stratified agrarian society — assigning occupations, regulating marriage, fixing ritual precedence — while securing material support and adjudicating authority for the priestly estate that composed and maintained the framework.
% FOUNDING_PROBLEM_CORROBORATION: The estate's own texts attest divine necessity, but no corroboration comes from the beneficiary set alone: anti-caste movement testimony documents the arrangement as functioning extraction, academic historiography of agrarian institutions traces the ordering function to concrete estate interests, and constitutional jurisprudence treats the arrangement as a rights violation rather than a needed order. Outside the benefiting parties, the attested reading is that whatever ordering problem once justified the framework, its persistence now tracks rent and status protection.
narrative_ontology:disappearance_verdict(jati_practice_norm__orthodox_textual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jati_practice_norm__orthodox_textual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jati_practice_norm__orthodox_textual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(jati_practice_norm__orthodox_textual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jati_practice_norm__orthodox_textual_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jati_practice_norm__orthodox_textual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jati_practice_norm__orthodox_textual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jati_practice_norm__orthodox_textual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is authored high (0.82 at interval end) because the arrangement transfers hereditary labor, service, and produce at rates set by custom rather than exchange, and blocks the mobility that would price that transfer. Suppression (0.62) is authored as the raw structural property it is — unscaled by power or scope; only extractiveness is scaled by directionality and scope in the engine's computation. It declines across the interval (0.88 to 0.62) tracking real enforcement decay: colonial courts, nationalist reform, and constitutional prohibition dismantled overt coercive machinery while extraction persisted through economic dependency and internalized doctrine — that divergence is the story's central temporal finding. Theater_ratio rises (0.20 to 0.48) as the theological justification grows more performative relative to function; public denial of untouchability alongside its private practice is the characteristic late-interval performance. Accessibility_collapse 0.70: alternatives such as conversion, migration, and trade shifts never fully collapsed but carried ruinous cost and traveling stigma. Resistance 0.55: sustained anti-caste movements, mass conversions, and everyday refusal met organized violent reprisal. Coalition note: the constraint grades status finely between victim strata — service jatis hold relative rank and reciprocal recognition above pollution-assigned jatis — which historically fragmented powerless-agent coalitions; durable cross-jati solidarity emerged only when that reward gradient was publicly rejected. Identity-lock note: the dalit seat's identity_locked exit fuses three mechanisms — doctrinal (karmic-desert teaching renders station self-deserved), relational (jati is marriage network, burial society, and mutual aid), and institutional (occupation, residence, and credit run through the hierarchy) — breaking any single frame alone does not open exit.
 *
 * PERSPECTIVAL GAP:
 *   From the brahmin_scriptural_authority seat the arrangement computes as cosmic order faithfully stewarded: the same texts that fix boundaries also assign the estate its duties, and enforcement appears as piety. From the dalit_pollution_assigned_jatis seat the identical structure computes as hereditary extraction under pollution terror: the texts that fix boundaries also fix who may be compelled, and enforcement appears as violence with liturgy. The landed patron seat sits between — it experiences the framework as flexible in practice, invoking obligation when cheap and hiring wage labor when cheaper, while depending on its rigidity for the labor supply. The engine computes these divergences from the structural data; the authored snare claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   brahmin_scriptural_authority declares beneficiary and administers: derivation places it near the beneficiary pole (low d) — the framework subsidizes it with adjudicating monopoly and dues. landed_upper_varna_patrons declares beneficiary with mobile exit: low-to-moderate d; the constraint subsidizes its labor supply on demand. dalit_pollution_assigned_jatis declares victim with identity_locked exit: near-full-target d — trapped or identity-locked targets sit nearest the full-target end. hereditary_service_jatis declares victim with constrained exit: high d, damped slightly by the reciprocal recognition the ranking grants them. bhakti_anticaste_movements are excluded rather than coordinated — they bear sanctions without receiving the framework's benefits, but as an authored absence they remain commentary-grade, never correction-grade. Scope amplification: the framework's claims are continental while verification of any local sanction is local, so the engine scales effective extraction accordingly.
 *
 * MANDATROPHY ANALYSIS:
 *   The snare claim prevents the coordination story from laundering extraction: a settled division of labor and a regulated marriage pool are real functions, but under this reading they are inseparable from hereditary transfer and pollution sanction, so the arrangement cannot certify as rope or tangled_rope without declaring who pays — and the payers are declared. The converse protection matters too: dismissing every function as extraction would miss why service jatis defended the ranking (guaranteed clients, ritual recognition) and would predict the wrong failure mode. Genealogy: the founding problem — ordering a stratified agrarian society while provisioning and empowering the scriptural estate — is contested; defenders attest divine necessity, while anti-caste testimony, historiography, and constitutional jurisprudence attest that the ordering function is now performed by markets, states, and voluntary association, leaving rent and status protection. Because founding_problem_status is authored contested rather than dead, the mismatch consumer does not mechanically flag a zombie; the temporal series nonetheless shows classic pre-mandatrophy drift — enforcement requirement falling while theater rises — and if extraction continues to fall while performance rises, the residue will be inertial performance, i.e., piton-shaped.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the jati_practice_norm kernel (orthodox_textual_reading). Would instantiating a sibling reading change the classification?',
    'Compare compiled classifications across the three sibling files: localized_practice_reading (renegotiable coordination norms) and colonial_census_reading (administrative reification) author their own epsilon, beneficiaries, and types; convergence or divergence across the family measures how much of caste classification is reading-indexed.',
    'If the localized reading computes as rope or tangled_rope while this reading computes as snare, the extraction is located in the fixity-and-pollution constitution itself rather than in jati differentiation as such; if all three compute alike, the kernel''s underlying arrangement dominates any reading of it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Reading-indexed classification: this story is the orthodox textual reading of the jati kernel; siblings instantiate different constraints.').

omega_variable(
    scriptural_fixity_vs_legitimating_overlay,
    'Does the scriptural varna framework actually generate lived jati boundaries, or does it operate as a legitimating overlay retrofitted onto a proliferating empirical system that resolves status by local negotiation?',
    'Code historical boundary disputes by adjudicating source: cases resolved by textual citation and estate adjudication versus cases resolved by local negotiation, marriage practice, and occupational change; correlate with region and period.',
    'If overlay dominates, the fixity claim is partly theatrical and extraction rides on selective textual enforcement — supporting the rising theater series and weakening the reading''s warrant; if textual adjudication genuinely binds, the constraint''s fixity is operative and the snare reading strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scriptural_fixity_vs_legitimating_overlay, empirical, 'Whether scriptural fixity is the operative generator of boundaries or retroactive legitimation.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (economic dependency, residential segregation, sanction violence) or internalized (pollution doctrine teaching assigned station as karmic desert)?',
    'Post-exit suppression trajectory: track converts and migrants — if pollution stigma and self-limitation persist after the structural mechanism is removed, reclassify the residual as internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure — the target carries the constraint after exit, and legal abolition alone cannot dissolve it; the enforcement-decay measurement series would overstate liberation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the pollution-sanction regime.').

omega_variable(
    naturalness_of_varna_order,
    'Is the varna framework a discovered cosmological order (as this reading claims) or a constructed charter serving identifiable estate interests?',
    'Comparative textual genealogy: trace the framework''s composition history against the estate''s material interests across redactions; test whether provisions track cosmological invariants or estate revenue.',
    'If constructed, categorical rigidity is policy-like and reformable, resistance is politics rather than impiety, and the reading''s theological axioms lose their warrant — the constraint certifies as a built arrangement rather than natural law.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_varna_order, conceptual, 'Natural-law versus constructed-charter status of the scriptural varna framework.').

omega_variable(
    enforcement_decay_persistence,
    'With formal enforcement legally abolished in the mid-twentieth century, what sustains the extraction — economic dependency lock-in, continued doctrinal belief, or covert sanction networks?',
    'Decompose post-abolition extraction by mechanism: wage and land-tenure data for dependency, belief surveys and practice audits for doctrine, incident reporting for covert sanction; compare regions differing in land-reform depth.',
    'If dependency dominates, the constraint is now economically self-enforcing and legal remedies under-treat it; if doctrine dominates, the internalized-suppression omega compounds; if covert sanction dominates, enforcement decay is overstated and the suppression series should flatten rather than fall.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_decay_persistence, empirical, 'Mechanism sustaining extraction after formal enforcement decay.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jati_practice_norm__orthodox_textual_reading, 1600, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jati_tr_t1600, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1600, 0.2).
narrative_ontology:measurement_basis(jati_tr_t1600, observed).
narrative_ontology:measurement(jati_tr_t1680, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1680, 0.22).
narrative_ontology:measurement_basis(jati_tr_t1680, observed).
narrative_ontology:measurement(jati_tr_t1760, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1760, 0.26).
narrative_ontology:measurement_basis(jati_tr_t1760, observed).
narrative_ontology:measurement(jati_tr_t1840, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1840, 0.33).
narrative_ontology:measurement_basis(jati_tr_t1840, observed).
narrative_ontology:measurement(jati_tr_t1920, jati_practice_norm__orthodox_textual_reading, theater_ratio, 1920, 0.4).
narrative_ontology:measurement_basis(jati_tr_t1920, observed).
narrative_ontology:measurement(jati_tr_t2000, jati_practice_norm__orthodox_textual_reading, theater_ratio, 2000, 0.48).
narrative_ontology:measurement_basis(jati_tr_t2000, observed).

% Extraction over time
narrative_ontology:measurement(jati_be_t1600, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1600, 0.9).
narrative_ontology:measurement_basis(jati_be_t1600, observed).
narrative_ontology:measurement(jati_be_t1680, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1680, 0.91).
narrative_ontology:measurement_basis(jati_be_t1680, observed).
narrative_ontology:measurement(jati_be_t1760, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1760, 0.89).
narrative_ontology:measurement_basis(jati_be_t1760, observed).
narrative_ontology:measurement(jati_be_t1840, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1840, 0.87).
narrative_ontology:measurement_basis(jati_be_t1840, observed).
narrative_ontology:measurement(jati_be_t1920, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 1920, 0.85).
narrative_ontology:measurement_basis(jati_be_t1920, observed).
narrative_ontology:measurement(jati_be_t2000, jati_practice_norm__orthodox_textual_reading, base_extractiveness, 2000, 0.82).
narrative_ontology:measurement_basis(jati_be_t2000, observed).

% Suppression requirement over time
narrative_ontology:measurement(jati_su_t1600, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1600, 0.88).
narrative_ontology:measurement_basis(jati_su_t1600, observed).
narrative_ontology:measurement(jati_su_t1680, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1680, 0.87).
narrative_ontology:measurement_basis(jati_su_t1680, observed).
narrative_ontology:measurement(jati_su_t1760, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1760, 0.84).
narrative_ontology:measurement_basis(jati_su_t1760, observed).
narrative_ontology:measurement(jati_su_t1840, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1840, 0.78).
narrative_ontology:measurement_basis(jati_su_t1840, observed).
narrative_ontology:measurement(jati_su_t1920, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 1920, 0.7).
narrative_ontology:measurement_basis(jati_su_t1920, observed).
narrative_ontology:measurement(jati_su_t2000, jati_practice_norm__orthodox_textual_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(jati_su_t2000, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jati_practice_norm__orthodox_textual_reading, identity_coordination).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__localized_practice_reading).
narrative_ontology:affects_constraint(jati_practice_norm__orthodox_textual_reading, jati_practice_norm__colonial_census_reading).

% DUAL FORMULATION NOTE:
% The colloquial label of the caste system covers at least three structurally distinct constraints (epsilon-invariance decomposition of the jati_practice_norm kernel): this file (orthodox_textual_reading) models boundaries as scripturally fixed with binding pollution sanctions — high epsilon, snare; jati_practice_norm__localized_practice_reading models boundaries as continuously renegotiated local coordination norms — lower epsilon, coordination-dominated; jati_practice_norm__colonial_census_reading models category stabilization by external administrative enumeration — extraction located in legibility-driven reification with the colonial state among beneficiaries. The upstream/downstream structure runs from the orthodox texts to colonial enumeration (the census drew its category inventory and intermediary class from the textual framework), while the localized reading contests both. Each file carries its own epsilon, stakeholder set, and claimed type; the edges here implement the family linkage.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
