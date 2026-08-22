% ============================================================================
% CONSTRAINT STORY: montevideo_statehood_criteria__declaratory_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_montevideo_statehood_criteria__declaratory_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: montevideo_statehood_criteria__declaratory_reading
 *   human_readable: Montevideo Criteria Declaratory Reading: Objective Statehood Test
 *   domain: international_law/political_philosophy
 *
 * SUMMARY:
 *   The Montevideo Convention on the Rights and Duties of States (1933)
 *   established four objective criteria for statehood: defined territory,
 *   permanent population, effective government, and capacity to conduct
 *   international relations. The declaratory reading holds that meeting these
 *   criteria CONSTITUTES statehood as a legal fact, independent of
 *   recognition by existing states. This reading competes with the
 *   constitutive reading (statehood requires recognition) and the hybrid
 *   reading (criteria are necessary but insufficient; legitimacy requirements
 *   like democratic governance also matter). Under the declaratory reading,
 *   de facto authorities meeting the Montevideo thresholds acquire legal
 *   standing and treaty rights without needing external approval, while
 *   parent states and gatekeeper powers lose the ability to condition
 *   recognition on political concessions. The constraint is CLAIMED as
 *   tangled_rope (it has genuine coordination function — establishing
 *   objective thresholds for system entry — and produces asymmetric
 *   extraction through the asymmetric loss of veto power) and is
 *   substantially enforced through international court opinions and
 *   diplomatic precedent. The reading is one instantiation of a contested
 *   kernel; sibling readings produce different beneficiary/victim structures
 *   and different statehood politics.
 *
 * KEY AGENTS:
 *   - de_facto_state_authorities — benefit from objective statehood test; lose identity_locked status only if parent recognizes them
 *   - parent_or_occupying_states — lose structural leverage to condition recognition; trapped exit options
 *   - established_state_community_gatekeepers — lose recognition as diplomatic tool; constrained exit to adapt treaty relationships
 *   - oppressed_nations_without_parent_recognition — benefit from legal claim independent of parent veto
 *   - international_court_system — agenda_setter; interprets whether Montevideo criteria are met and applies the declaratory rule
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, 0.62).
domain_priors:suppression_score(montevideo_statehood_criteria__declaratory_reading, 0.71).
domain_priors:theater_ratio(montevideo_statehood_criteria__declaratory_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, extractiveness, 0.62).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, accessibility_collapse, 0.48).
narrative_ontology:constraint_metric(montevideo_statehood_criteria__declaratory_reading, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(montevideo_statehood_criteria__declaratory_reading, tangled_rope).
narrative_ontology:human_readable(montevideo_statehood_criteria__declaratory_reading, "Montevideo Criteria Declaratory Reading: Objective Statehood Test").
narrative_ontology:topic_domain(montevideo_statehood_criteria__declaratory_reading, "international_law/political_philosophy").

domain_priors:requires_active_enforcement(montevideo_statehood_criteria__declaratory_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(montevideo_statehood_criteria__declaratory_reading, 'e5545cab-82e4-4df5-9353-820316f0fe42').
narrative_ontology:cs_kernel_codification('e5545cab-82e4-4df5-9353-820316f0fe42', fixed_text).
narrative_ontology:cs_authority_grounding('e5545cab-82e4-4df5-9353-820316f0fe42', lineage).
narrative_ontology:cs_interpretation_layer_present('e5545cab-82e4-4df5-9353-820316f0fe42').
narrative_ontology:cs_reading_relation('e5545cab-82e4-4df5-9353-820316f0fe42', montevideo_statehood_criteria__constitutive_reading, forecloses).
narrative_ontology:cs_reading_relation('e5545cab-82e4-4df5-9353-820316f0fe42', montevideo_statehood_criteria__hybrid_reading, influences).
narrative_ontology:cs_axiom('e5545cab-82e4-4df5-9353-820316f0fe42', foundational, statehood_criteria_sufficient).
narrative_ontology:cs_axiom_status(statehood_criteria_sufficient, holdable).
narrative_ontology:cs_axiom_grounding('e5545cab-82e4-4df5-9353-820316f0fe42', statehood_criteria_sufficient, conventional).
narrative_ontology:cs_axiom('e5545cab-82e4-4df5-9353-820316f0fe42', foundational, recognition_ceremonial_not_constitutive).
narrative_ontology:cs_axiom_status(recognition_ceremonial_not_constitutive, holdable).
narrative_ontology:cs_axiom_grounding('e5545cab-82e4-4df5-9353-820316f0fe42', recognition_ceremonial_not_constitutive, deontological).
narrative_ontology:cs_reference_frame('e5545cab-82e4-4df5-9353-820316f0fe42', objective_statehood_by_criteria).
narrative_ontology:cs_drift_state('e5545cab-82e4-4df5-9353-820316f0fe42', contemporary_recognition_crisis, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('e5545cab-82e4-4df5-9353-820316f0fe42', '').
narrative_ontology:cs_kernel_id(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, de_facto_state_authorities).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, oppressed_nations_without_parent_recognition).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, self_determination_movements).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_or_occupying_states).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, established_state_community_gatekeepers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(montevideo_statehood_criteria__declaratory_reading, parent_state_diaspora_communities).
narrative_ontology:constraint_victim(montevideo_statehood_criteria__declaratory_reading, parent_state_diaspora_communities).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, international_law_self_executing).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, state_sovereignty_fact_based).
narrative_ontology:constraint_vindicates(montevideo_statehood_criteria__declaratory_reading, self_determination_overrides_consent).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Political and military leadership of a territory that meets the Montevideo criteria (defined, permanent population, effective government, capacity for international relations) but is denied recognition by the parent state or major powers. Under this reading, they acquire legal statehood status without needing external recognition — their authority is vindicated by meeting objective thresholds rather than requiring consensus approval from existing states. They can access international legal standing, claim treaty rights, and resist interference based on statehood, not as rebels or separatists.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, de_facto_state_authorities, beneficiary,
    organized, generational, identity_locked, regional).

% Peoples and populations with distinct territory, government, and will to statehood whose parent or occupying state refuses recognition and pressures other states to withhold it. This reading's constraint grants them a structural claim to statehood independent of parent-state veto. They benefit because international law becomes a floor, not a ceiling — whether or not powerful states recognize them, their statehood is a legal fact if they meet the objective criteria.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, oppressed_nations_without_parent_recognition, beneficiary,
    moderate, generational, constrained, global).

% Independence and liberation movements seeking statehood for their territories. The declaratory reading removes the parent state's veto from the statehood question — they can argue in international forums that they ARE a state de jure once they meet the Montevideo thresholds, rather than begging existing states for recognition.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, self_determination_movements, beneficiary,
    moderate, biographical, constrained, global).

% Existing sovereign states that contain territories with de facto governments meeting objective statehood criteria but wish to preserve territorial integrity. Under the declaratory reading, they lose structural leverage to condition recognition on political concessions — the territory's statehood status becomes a legal fact whether they approve or not. They bear costs: loss of formal control, inability to blockade recognition, loss of the strategic tool of conditional recognition, and pressure to treat the territory as a state for treaty and diplomatic purposes even if they dispute its legitimacy.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_or_occupying_states, payer,
    institutional, generational, trapped, global).

% Permanent Security Council members, major regional powers, and states that use recognition politics as a tool of leverage. The declaratory reading strips them of a key instrument: they can no longer withhold recognition as punishment or demand concessions in exchange for it. A territory that meets the Montevideo criteria is a state de jure, rendering their recognition/non-recognition distinction legally moot. They bear the cost of losing this instrument of statecraft and of adjusting diplomatic and treaty relationships to accommodate unrecognized but legally-fact states.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, established_state_community_gatekeepers, payer,
    institutional, generational, constrained, global).

% Communities originating in the parent state but residing in the de facto territory or with kinship ties to it. They bear costs if the territory's statehood is recognized — loss of citizenship continuity with the parent, divided family nationality, changed legal status. They also have potential benefit if they retain ties to the territory: access to a new state's services and protection.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, parent_state_diaspora_communities, payer,
    moderate, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(montevideo_statehood_criteria__declaratory_reading, parent_state_diaspora_communities, beneficiary).

% Academic and institutional authorities on international law who interpret and refine the statehood doctrine. They witness the constraint's operation and help adjudicate whether the Montevideo criteria are met in specific cases — they are not beneficiaries or payers, but their analysis shapes how the reading is applied.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_law_scholars_and_institutions, observer,
    analytical, generational, analytical, global).

% International Court of Justice, regional courts, and international arbitration bodies that interpret whether the Montevideo criteria are satisfied and apply the declaratory rule. They administer the constraint by rendering binding opinions on whether de facto governments have met the threshold for statehood and whether states must treat them as legal equals.
narrative_ontology:constraint_stakeholder(montevideo_statehood_criteria__declaratory_reading, international_court_system, agenda_setter,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(montevideo_statehood_criteria__declaratory_reading, de_facto_state_authorities).
narrative_ontology:fixing_cost_class(montevideo_statehood_criteria__declaratory_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes an objective, law-based standard for statehood that does not depend on consensus approval by existing states. Solves the coordination problem of how new states enter the international system: instead of requiring unanimous consent or coalition agreement, the declaratory reading provides a threshold test that any political authority can reference to claim legal statehood if it meets the criteria.
% TRANSFER_FUNCTION: Transfers legitimacy and legal standing from recognition (a gift existing states can grant or withhold) to objective criteria (a structural fact no single state can deny). Parent states and gatekeeper states lose the power to condition statehood on political concessions; de facto authorities and self-determination movements gain a legal claim independent of external approval.
% ABSENT_VOICES: Minority populations within de facto territories are not explicitly represented; they would argue for protection guarantees or autonomy options but are often structured out of the statehood debate. Stateless peoples not meeting the Montevideo criteria are also absent — their concerns would be that objective thresholds exclude legitimate claims based on historical wrongs or cultural identity rather than de facto governmental control.
% DISAPPEARANCE_RATIONALE: If this constraint (the declaratory rule) disappeared, the international legal system would revert to purely constitutive statehood: territories with de facto governments would have no standing to claim legal statehood unless existing states voted to recognize them. Kosovo, Taiwan, Northern Cyprus, Palestine, and similar cases would become pure political questions without legal thresholds, and parent states and powerful gatekeepers would recover their veto power over territorial claims. The absence of the declaratory rule collapses objective law into politics.
% FOUNDING_PROBLEM: The constitutive reading (statehood requires recognition) gave existing states unchecked veto power over new state admission, enabling perpetual denial of statehood for territories meeting all practical criteria of governance and self-determination. The declaratory reading was formulated to answer: how can international law guarantee that objective governmental and territorial facts matter for statehood, not merely the political will of existing powers?
% FOUNDING_PROBLEM_CORROBORATION: De facto authorities in territories like Kosovo and Palestine have appealed to the Montevideo criteria as grounding independent statehood claims, citing sources outside the parent or gatekeeping states. International law scholarship (including from the International Court of Justice) has acknowledged the declaratory principle as a competing reading of statehood doctrine. Self-determination advocates and independence movements corroborate that the founding problem persists: parent states routinely deny recognition even where objective criteria are met, and the declaratory reading is used as a counterargument to that veto.
narrative_ontology:disappearance_verdict(montevideo_statehood_criteria__declaratory_reading, world_rearranges).
narrative_ontology:founding_problem_status(montevideo_statehood_criteria__declaratory_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(montevideo_statehood_criteria__declaratory_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(montevideo_statehood_criteria__declaratory_reading, 'none', 1).
narrative_ontology:epsilon_provenance(montevideo_statehood_criteria__declaratory_reading, 0.62, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(montevideo_statehood_criteria__declaratory_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(montevideo_statehood_criteria__declaratory_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(montevideo_statehood_criteria__declaratory_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate-high (0.62 at interval end) because the declaratory reading systematically strips gatekeeping power from existing states and parent states — a structural asymmetry. They lose legitimacy to deny statehood based on political preference alone. Suppression is higher (0.71) because maintaining the constitutive alternative requires active institutional work: courts must resist the declaratory reading, states must enforce non-recognition despite meeting criteria, and diplomatic pressure must be maintained to prevent de facto authorities from claiming legal statehood. Theater ratio rises over the interval (0.28 to 0.41) as the constraint matures: early enforcement focuses on real disputes about whether criteria are met; later enforcement increasingly involves performative non-recognition (states denying legal statehood despite acknowledging factual governance) and diplomatic theater around 'conditional recognition.' Accessibility collapse is low-moderate (0.48) because alternatives persist: parent states can still argue for constitutive statehood and withhold recognition; the declaratory reading does not eliminate the political contest, only shifts its basis. Resistance is high (0.68) because the constraint faces active pushback from states that benefit from recognition veto and from legal scholars defending constitutive doctrine. The measurements are on one shared time grid; the series runs from early formalization of the declaratory reading (Montevideo and subsequent ICJ opinions) through contemporary de facto statehood claims.
 *
 * PERSPECTIVAL GAP:
 *   The de facto authority seat sees this constraint as liberation — a legal fact they can point to for equal standing. The parent state seat sees it as expropriation of its sovereignty instrument. The gatekeeper seat sees it as erosion of concert-of-states privilege. The international court seat (observer) sees it as a technical question of whether four objective thresholds are met. The engine computes these divergent readings from the structural data (beneficiary/victim + power + exit + scope); the constraint's claimed type (tangled_rope) encodes the author's structural hypothesis that this is genuinely coordinative (objective statehood test) AND systematically extractive (strips existing states of veto power), making it a hybrid rather than pure rope.
 *
 * DIRECTIONALITY LOGIC:
 *   De facto authorities are beneficiaries with constrained-to-identity_locked exit: they gain legal standing but cannot easily become something else (they are bound to their territory and people). Their directionality is near the beneficiary end (d ~ 0.2-0.3). Parent and gatekeeper states are victims with trapped exit: they lose power and cannot easily recover it short of overturning international law. Their directionality is near the target end (d ~ 0.7-0.8). The asymmetry is structural: the constraint consolidates power downward (from gatekeepers to de facto authorities) and cannot be reciprocal. A directionality override is NOT needed here; the structural derivation captures the true relationship.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (parent-state veto prevents statehood for territories meeting all criteria) is live: current cases (Palestine, Kosovo, Taiwan, Northern Cyprus) continue to exhibit non-recognition despite objective governance. The constraint's mandate to establish objective thresholds for statehood remains active, not atrophied. However, theater ratio is rising, which signals potential degradation: diplomatic ritual around statehood recognition increasingly substitutes for the coordination function. As theater approaches 0.5, the engine would flag a potential piton conversion (function atrophied but theater persists). The story's measurements show extraction stabilizing around 0.62, which is consistent with a tangled_rope in equilibrium — not rising toward snare, not falling toward rope. Mandatrophy is not yet resolved; the constraint remains an active hybrid coordinative-extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    declarative_vs_constitutive_kernel_contest,
    'Does meeting the Montevideo criteria MAKE a territory a state (declaratory), or does recognition by existing states MAKE it a state (constitutive)?',
    'This is a conceptual/constitutional choice, not an empirical question resolvable by data. The resolution is settled by which reading wins acceptance in international courts, state practice, and treatymaking. The declaratory reading is instantiated as a constraint BECAUSE this choice has not been universally resolved.',
    'If the declaratory reading prevails, de facto authorities enter the victim set of parent states and gatekeepers (they lose veto power). If the constitutive reading prevails, de facto authorities remain perpetually powerless without external recognition. If the hybrid reading prevails, criteria become necessary but gatekeepers recover a legitimacy gate (and can condition recognition on governance standards).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(declarative_vs_constitutive_kernel_contest, conceptual, 'Whether statehood is a fact (declaratory), a grant (constitutive), or a judgment (hybrid).').

omega_variable(
    montevideo_criteria_interpretation_drift,
    'Do the four Montevideo criteria have a stable interpretation, or do changing state practice and technological conditions (digital territories, virtual governance, climate migration) alter what ''defined territory,'' ''permanent population,'' and ''effective government'' mean?',
    'Monitor how international courts apply the criteria to novel cases (digital governance, stateless territories, populations in diaspora). If criteria expand to accommodate new forms of political organization, the constraint''s scope widens and extraction pressure increases on gatekeepers. If criteria remain rigid, new actors are excluded by definition.',
    'Stable interpretation keeps the constraint''s beneficiary set bounded (only traditional de facto governments). Drift expands the beneficiary set (new forms of political organization gain claims to statehood) and increases gatekeeper suppression pressure (more entities demanding recognition).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(montevideo_criteria_interpretation_drift, empirical, 'Whether the Montevideo criteria evolve or remain fixed as state practice and technology change.').

omega_variable(
    effective_government_verification_problem,
    'Who determines whether a de facto authority meets the ''effective government'' criterion, and by what standard?',
    'International courts and state practice establish verification standards over time. If verification is strict (courts demand functional state capacity, human rights compliance, territorial control without coercion), few de facto governments qualify. If verification is lenient (courts accept any organized government structure that exercises de facto control), many more qualify.',
    'Strict verification keeps the constraint''s beneficiary set small and gatekeepers retain power. Lenient verification expands beneficiary claims and increases extraction pressure on gatekeepers to treat de facto entities as states.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effective_government_verification_problem, empirical, 'Whether ''effective government'' is a high or low bar for Montevideo criterion satisfaction.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression (0.71) structural (gatekeepers must actively maintain non-recognition through institutional work) or internalized (de facto authorities have internalized doubt about their own legitimacy)?',
    'Post-exit suppression trajectory: if a de facto authority gains substantial international recognition and still reports suppression effects, suppression is partly internalized. If suppression disappears upon recognition, it was purely structural.',
    'If structural, the constraint''s enforcement requires active institutional work and could be dismantled by changing international law. If internalized, suppression persists even after de facto authorities gain legal status — they carry the delegitimation with them.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Whether suppression of de facto statehood claims is a structural gatekeeping mechanism or an internalized identity wound.').

omega_variable(
    kernel_reading_foreclosure_or_coexistence,
    'Can the declaratory, constitutive, and hybrid readings of the Montevideo kernel coexist as live positions within a single international legal framework, or does the declaratory reading logically foreclose the constitutive and hybrid readings?',
    'If courts have accepted multiple readings and applied them to different cases (showing internal contradiction), then the readings coexist. If one reading has systematically driven out the others in authoritative venues, foreclosure has occurred.',
    'Coexistence means the constraint lives in a contested legal space where multiple readings apply to different cases. Foreclosure means the declaratory reading has become canonical and the constitutive/hybrid readings are residual.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure_or_coexistence, conceptual, 'Whether the kernel''s readings are mutually exclusive or simultaneously live.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(montevideo_statehood_criteria__declaratory_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(montevideo_declaratory_tr_t0, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(montevideo_declaratory_tr_t0, observed).
narrative_ontology:measurement(montevideo_declaratory_tr_t8, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(montevideo_declaratory_tr_t8, observed).
narrative_ontology:measurement(montevideo_declaratory_tr_t16, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(montevideo_declaratory_tr_t16, observed).
narrative_ontology:measurement(montevideo_declaratory_tr_t25, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 25, 0.4).
narrative_ontology:measurement_basis(montevideo_declaratory_tr_t25, observed).
narrative_ontology:measurement(montevideo_declaratory_tr_t37, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 37, 0.41).
narrative_ontology:measurement_basis(montevideo_declaratory_tr_t37, projected).
narrative_ontology:measurement(montevideo_declaratory_tr_t50, montevideo_statehood_criteria__declaratory_reading, theater_ratio, 50, 0.41).
narrative_ontology:measurement_basis(montevideo_declaratory_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(montevideo_declaratory_be_t0, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 0, 0.48).
narrative_ontology:measurement_basis(montevideo_declaratory_be_t0, observed).
narrative_ontology:measurement(montevideo_declaratory_be_t8, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 8, 0.54).
narrative_ontology:measurement_basis(montevideo_declaratory_be_t8, observed).
narrative_ontology:measurement(montevideo_declaratory_be_t16, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 16, 0.59).
narrative_ontology:measurement_basis(montevideo_declaratory_be_t16, observed).
narrative_ontology:measurement(montevideo_declaratory_be_t25, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 25, 0.61).
narrative_ontology:measurement_basis(montevideo_declaratory_be_t25, observed).
narrative_ontology:measurement(montevideo_declaratory_be_t37, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 37, 0.62).
narrative_ontology:measurement_basis(montevideo_declaratory_be_t37, projected).
narrative_ontology:measurement(montevideo_declaratory_be_t50, montevideo_statehood_criteria__declaratory_reading, base_extractiveness, 50, 0.62).
narrative_ontology:measurement_basis(montevideo_declaratory_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(montevideo_declaratory_su_t0, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 0, 0.58).
narrative_ontology:measurement_basis(montevideo_declaratory_su_t0, observed).
narrative_ontology:measurement(montevideo_declaratory_su_t8, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 8, 0.63).
narrative_ontology:measurement_basis(montevideo_declaratory_su_t8, observed).
narrative_ontology:measurement(montevideo_declaratory_su_t16, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 16, 0.67).
narrative_ontology:measurement_basis(montevideo_declaratory_su_t16, observed).
narrative_ontology:measurement(montevideo_declaratory_su_t25, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 25, 0.7).
narrative_ontology:measurement_basis(montevideo_declaratory_su_t25, observed).
narrative_ontology:measurement(montevideo_declaratory_su_t37, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 37, 0.71).
narrative_ontology:measurement_basis(montevideo_declaratory_su_t37, projected).
narrative_ontology:measurement(montevideo_declaratory_su_t50, montevideo_statehood_criteria__declaratory_reading, suppression_requirement, 50, 0.71).
narrative_ontology:measurement_basis(montevideo_declaratory_su_t50, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(montevideo_statehood_criteria__declaratory_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(montevideo_statehood_criteria__declaratory_reading, 0.12).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__constitutive_reading).
narrative_ontology:affects_constraint(montevideo_statehood_criteria__declaratory_reading, montevideo_statehood_criteria__hybrid_reading).

% DUAL FORMULATION NOTE:
% The three constraint stories in the montevideo_statehood_criteria kernel family represent three competing legal readings of the same founding text. The declaratory reading removes parent-state veto power by establishing that objective Montevideo criteria suffice for statehood as a legal fact. The constitutive reading preserves parent-state veto by requiring external recognition. The hybrid reading adds legitimacy gates (democratic governance, human rights) beyond the criteria. Each reading instantiates a different constraint with different ε values (declaratory is most extractive because it strips gatekeeping power), different beneficiary/victim structures, and different enforcement requirements. The three stories are linked as a constraint family — they share a kernel (the Montevideo Convention) but produce structurally distinct constraints when instantiated under different readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(montevideo_statehood_criteria__declaratory_reading, institutional, 0.75).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
