% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__defensive_spiritual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__defensive_spiritual_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__defensive_spiritual_reading
 *   human_readable: Jihad as Defensive Spiritual Struggle with Armed Response Constraints
 *   domain: religious/legal/political
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of the contested Qur'anic jihad
 *   corpus: jihad as primarily internal spiritual struggle (jihad al-nafs)
 *   with armed response strictly limited to defense against aggression,
 *   requiring state authorization, proportional response, and non-combatant
 *   immunity. This is the reading authoritatively held by classical
 *   jurisprudential schools (all four Sunni madhhabs), contemporary
 *   mainstream Islamic scholarship, and Islamic reform movements. The reading
 *   is not presented as natural law but as a coherent interpretation that
 *   privileges certain Qur'anic verses and jurisprudential principles over
 *   others. The constraint's extractiveness is LOW (0.32) because this
 *   reading imposes constraints ON armed action rather than extracting
 *   through it: it requires state authorization (gates individual
 *   initiative), demands proportionality (limits response scope), and
 *   mandates non-combatant protection (restricts target set). The suppression
 *   score reflects the active maintenance required to enforce these
 *   constraints against sibling readings that claim different authority
 *   structures and thresholds. Theater is very low (0.12): the constraint's
 *   functional purpose (reconciling inner struggle with legitimate defense)
 *   remains its actual purpose; there is minimal performative maintenance
 *   compared to pitons or snares.
 *
 * KEY AGENTS:
 *   - legitimate_islamic_states — agenda-setter institutional actors controlling authorization for armed jihad
 *   - islamic_jurisprudential_authorities — institutional interpreters of the corpus and enforcers of constraint thresholds
 *   - muslim_spiritual_practitioners — beneficiaries of the constraint's privileging of internal struggle
 *   - non_muslim_populations_outside_aggression — beneficiaries of non-combatant immunity principle
 *   - revolutionary_vanguard_interpreters — excluded by this reading's state-requirement gate; claim different authority structure
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__defensive_spiritual_reading, 0.32).
domain_priors:suppression_score(jihad_quranic_corpus__defensive_spiritual_reading, 0.28).
domain_priors:theater_ratio(jihad_quranic_corpus__defensive_spiritual_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, accessibility_collapse, 0.68).
narrative_ontology:constraint_metric(jihad_quranic_corpus__defensive_spiritual_reading, resistance, 0.71).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__defensive_spiritual_reading, rope).
narrative_ontology:human_readable(jihad_quranic_corpus__defensive_spiritual_reading, "Jihad as Defensive Spiritual Struggle with Armed Response Constraints").
narrative_ontology:topic_domain(jihad_quranic_corpus__defensive_spiritual_reading, "religious/legal/political").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__defensive_spiritual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__defensive_spiritual_reading, 'c0d18fc5-af5c-4a86-b680-e1e1ed7c0007').
narrative_ontology:cs_kernel_codification('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', fixed_text).
narrative_ontology:cs_authority_grounding('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', lineage).
narrative_ontology:cs_interpretation_layer_present('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007').
narrative_ontology:cs_reading_relation('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', jihad_quranic_corpus__expansionist_legalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', jihad_quranic_corpus__revolutionary_vanguard_reading, forecloses).
narrative_ontology:cs_axiom('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', foundational, jihad_primarily_internal_spiritual_struggle).
narrative_ontology:cs_axiom_status(jihad_primarily_internal_spiritual_struggle, holdable).
narrative_ontology:cs_axiom_grounding('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', jihad_primarily_internal_spiritual_struggle, deontological).
narrative_ontology:cs_axiom('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', foundational, armed_action_requires_state_authority_and_defensive_necessity).
narrative_ontology:cs_axiom_status(armed_action_requires_state_authority_and_defensive_necessity, holdable).
narrative_ontology:cs_axiom_grounding('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', armed_action_requires_state_authority_and_defensive_necessity, conventional).
narrative_ontology:cs_axiom('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', secondary, non_combatant_immunity_universal_principle).
narrative_ontology:cs_axiom_status(non_combatant_immunity_universal_principle, holdable).
narrative_ontology:cs_axiom_grounding('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', non_combatant_immunity_universal_principle, deontological).
narrative_ontology:cs_reference_frame('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', quranic_spiritual_emphasis_with_legitimate_defense).
narrative_ontology:cs_drift_state('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', contemporary_conflict_era_post_2001, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c0d18fc5-af5c-4a86-b680-e1e1ed7c0007', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_practicing_spiritual_jihad).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, islamic_jurisprudential_tradition).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_islamic_states).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_spiritual_practitioners).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, islamic_jurisprudential_authorities).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_populations_outside_aggression).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_under_aggression).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__defensive_spiritual_reading, contemporary_islamic_reformists).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, quranic_emphasis_on_intention_and_inner_struggle).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, proportionality_constraint_in_armed_response).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, non_combatant_immunity_principle).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__defensive_spiritual_reading, state_authority_requirement_for_legitimate_armed_action).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Engage in the internal spiritual struggle (jihad al-nafs) against ego, material attachment, and moral failing. This reading privileges this dimension and provides doctrinal support for understanding jihad primarily through personal ethics, Qur'anic interpretation, and disciplined practice. They frame the armed component as a rare, constrained exception rather than the central meaning.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_spiritual_practitioners, beneficiary,
    moderate, biographical, mobile, global).

% Claim exclusive authority to declare and conduct armed jihad in defense of territory, population, and Islamic order. This reading requires state authorization, established legal process (ijma' of scholars, consultation with muftis), clear defensive necessity, and proportional response. They set the gate that determines when the armed dimension activates.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, legitimate_islamic_states, agenda_setter,
    institutional, generational, constrained, national).

% Maintain and interpret the jurisprudential tradition, determine when armed response meets the constraint thresholds (defensive necessity, proportionality, non-combatant protection), and issue binding rulings (fatwa). They interpret the corpus and enforce the reading's conditions. Their authority depends on the reading's coherence and their credibility in applying its standards.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, islamic_jurisprudential_authorities, agenda_setter,
    institutional, generational, analytical, global).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__defensive_spiritual_reading, islamic_jurisprudential_authorities, beneficiary).

% Are excluded from the victim/target set of legitimate armed jihad under this reading UNLESS they are aggressors against Muslim communities. They benefit from the constraint's non-combatant immunity principle, which frames armed response as lawful only against combatants actively engaged in aggression, not against non-Muslims generally. Coexistence is the privileged framework.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, non_muslim_populations_outside_aggression, beneficiary,
    moderate, biographical, mobile, global).

% Claim the right to armed defense when facing external aggression. This reading supports their right to resist but constrains it: response must be proportional, authorized by legitimate state structure, and must distinguish combatants from non-combatants. The constraint both grants and limits their recourse.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, muslim_communities_under_aggression, beneficiary,
    powerless, biographical, trapped, local).

% Hold that jihad includes obligation to establish Islamic governance where absent (though with jurisprudential conditions: invitation first, imam authority, proportionality). They are excluded from the primary interpretation gate by this reading's framing of jihad as primarily spiritual and armed action as defensive only. Their voices are present in the corpus but subordinated by this reading's structural choices.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, expansionist_legalist_scholars, excluded,
    institutional, generational, analytical, global).

% Hold that jihad is an immediate individual obligation against apostate rulers and occupiers, bypassing state authority through emergency jurisprudence and takfir. They are explicitly excluded from authority under this reading, which requires state structure and rejects the emergency bypass. Their claim to individual obligation (fard 'ayn) contradicts this reading's state-requirement gate.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, revolutionary_vanguard_interpreters, excluded,
    powerless, immediate, identity_locked, local).

% Study the historical development of jihad interpretation across Islamic jurisprudential schools (madhhabs) and contemporary Islamic movements. They observe that this reading privileges certain Qur'anic verses (those emphasizing inner struggle, proportionality, non-combatant status) while acknowledging others exist and are read differently by sibling interpretations.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, quranic_corpus_interpreters, observer,
    analytical, civilizational, analytical, universal).

% Emphasize Islam's capacity to coexist with non-Muslim polities and pluralistic frameworks. This reading supports their position by constraining armed action, privileging spiritual meaning, requiring state authority, and universalizing non-combatant protection. They benefit from the reading's structural emphasis on coexistence.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__defensive_spiritual_reading, contemporary_islamic_reformists, beneficiary,
    moderate, biographical, mobile, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__defensive_spiritual_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__defensive_spiritual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for understanding Islamic duty (fard) that coordinates individual spiritual practice with collective defense: jihad as primarily internal moral struggle legitimates peacetime Islamic life while armed response remains available under strict conditions for defensive necessity. Resolves potential conflict between Islamic ethics and self-defense by making both intelligible within a single interpretive tradition.
% TRANSFER_FUNCTION: Moves authority over armed action from individuals to state institutions; moves interpretive authority from unaccountable actors to established jurisprudential bodies; moves the primary locus of jihad from external military campaigns to internal spiritual discipline. The constraint's effect is redistribution of authorization power, not material flow.
% ABSENT_VOICES: Revolutionary movements and radical interpreters are structurally excluded (role=excluded): they claim individual obligation and emergency authority, which this reading's state-requirement gate specifically forecloses. Their interpretation of the same corpus is present in Islamic history but subordinated by this reading's structural choices. Expansionist legalist interpreters are also excluded: their framing of jihad as governance-establishment obligation is subordinated to the spiritual-priority framing.
% DISAPPEARANCE_RATIONALE: If this constraint disappeared and were replaced by the expansionist or revolutionary readings, the legitimacy conditions for armed action would shift dramatically: state requirement would dissolve, individual obligation would activate, and the victim set would expand beyond combatants to include non-Muslim polities or apostate rulers and their supporters. Muslims would reorganize around different authorities (scholars issuing emergency fatwas vs. state institutions) and different thresholds for action (defensive necessity vs. expansion of Islamic governance). The constraint's disappearance would reorganize Islamic jurisprudence itself.
% FOUNDING_PROBLEM: The Qur'anic corpus contains verses emphasizing both inner spiritual struggle (jihad al-nafs) and armed response to external aggression, and these must be reconciled within a coherent jurisprudential framework that neither ignores either dimension nor reduces both to military campaign. The founding problem is theological: how to honor the full range of Qur'anic teaching while producing a stable, interpretable law.
% FOUNDING_PROBLEM_CORROBORATION: Classical and contemporary Islamic jurisprudential traditions across madhhabs (Hanafi, Maliki, Shafi'i, Hanbali) and modern Islamic scholars (al-Qaradawi, Ramadan, Abou El Fadl, an-Na'im) attest the problem is live and contested. Qur'anic exegetes from the 9th century onward have grappled with reconciling verses on inner struggle and armed response. The problem is NOT attested only by this reading's beneficiaries; it is a standing debate in Islamic scholarship visible in every major jurisprudential school, contemporary Islamic discourse, and academic Islamic studies.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__defensive_spiritual_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__defensive_spiritual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__defensive_spiritual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__defensive_spiritual_reading, 0.32, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).
:- end_tests(jihad_quranic_corpus__defensive_spiritual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.32) because this reading redistributes authority (from individuals to states; from emergency interpreters to established jurisprudence) but does not extract material rents. The constraint LIMITS the scope of authorized action rather than expanding it for benefit of any actor. Non-Muslims outside aggression are explicitly OUTSIDE the victim set under this reading — they are beneficiaries of the non-combatant immunity principle. The measurement series is flat to slightly rising over 40 years, indicating stable low extractiveness with minor uptick in theater (from 0.08 to 0.12) as contemporary Islamic movements face pressure to clarify the reading's application in conflict contexts. Suppression rises modestly (0.22 to 0.28) because revolutionary and expansionist interpretations maintain countervailing authority claims; this reading must actively enforce its state-requirement gate and proportionality thresholds against live competing readings. The accessibility_collapse score (0.68) reflects that alternatives (revolutionary fard 'ayn, expansionist establishment of Islamic governance) DO remain accessible through the same corpus; this reading does not achieve the near-complete collapse of alternatives that a mountain would show (0.85+). Resistance is high (0.71) because the competing readings have active proponents and institutional bases — the revolutionary vanguard reading has inspired actual movements; the expansionist reading is held by authoritative scholars. This constraint persists not because alternatives are unthinkable but because a powerful coalition (state institutions, classical jurisprudence, contemporary reformists) maintains it actively.
 *
 * PERSPECTIVAL GAP:
 *   From the legitimate state institutional seat, this reading is genuine coordination: it provides them with jurisprudential justification for monopolizing armed response decisions and protects them against unauthorized individual claims to armed jihad. From the revolutionary vanguard seat (excluded), it is a foreclosure mechanism: their claim to individual obligation (fard 'ayn) is ruled out by the state-requirement gate, and they experience the constraint as suppression of their authority to interpret in emergency contexts. The engine computes this divergence from the structural data: same constraint, opposite directionality (d ≈ 0.2 for states, d ≈ 0.85 for excluded revolutionary interpreters). The payer/beneficiary seats' differing exit options drive the divergence: institutional states have arbitrage (they can reinterpret, move to a different authority base, shift jurisprudential authority) while revolutionary interpreters are identity_locked (their self-conception depends on claiming direct access to Qur'anic authority and emergency legitimacy). The constraint's persistence depends on the institutional actors maintaining it actively; the excluded actors resist it but lack the institutional base to displace it within classical jurisprudential schools.
 *
 * DIRECTIONALITY LOGIC:
 *   Legitimate Islamic states are beneficiaries (role=agenda_setter): they monopolize authorization for armed action and use established jurisprudence to validate their authority over emergency claims. Spiritual practitioners are beneficiaries (role=beneficiary): the reading privileges their dimension of jihad and provides doctrinal support for peacetime Islamic life. Islamic jurisprudential authorities are beneficiaries/co-agenda-setters (role=agenda_setter, secondary_role=beneficiary): their credibility and institutional power rest on the reading's coherence; they gain authority relative to unaccountable individual interpreters. Non-Muslims outside aggression are beneficiaries: the non-combatant immunity principle protects them specifically. The revolutionary interpreters are EXCLUDED (role=excluded): their claim to individual obligation and emergency authority is foreclosed by this reading's state-requirement gate. There are no 'payers' in the traditional sense — no group bears extraction costs through this constraint. The constraint reallocates authority (a form of power redistribution) but does not extract material flows. This is consistent with a rope-type coordination: it solves the founding problem (reconciling inner struggle and legitimate defense) with genuine coordination benefit and no systematic victimization. The directionality is nearly symmetric to beneficiary-skewed because the constraint's primary effect is authorization redistribution, not material extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading is NOT mandatrophic. The founding problem it was built to solve (reconciling inner spiritual struggle with legitimate armed response in a coherent jurisprudential framework) remains live, contested, and central to Islamic theological and political discourse. The constraint persists because it solves that problem in a way that powerful institutional actors (states, classical schools, contemporary reformists) find coherent and useful. Alternative readings (expansionist, revolutionary) also exist and address the same founding problem, but they are not the answer THIS reading provides. The measurement series shows stable extractiveness and low theater — no sign of performance replacing function. Mandatrophy would appear as either: (1) the founding problem becoming manifestly solved or dead (it is not — jihad interpretation remains contested), or (2) theater_ratio rising sharply as the constraint persists despite losing functional purpose (not observed). The constraint remains mandatorily needed by the coalition that maintains it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    state_authority_legitimacy_basis,
    'What grounds the requirement for state authority in legitimate armed jihad — Qur''anic command, classical jurisprudential consensus, or pragmatic institutional stability? Is this requirement inherent to Islamic law or a contingent historical development?',
    'Genealogical study of jurisprudential development: trace when state-authority requirement entered the mainstream tradition (9th-10th centuries in Sunni schools) and whether it was grounded in Qur''anic exegesis or institutional interest. Compare with Qur''anic language on ''those in authority'' (ulil-amr, 4:59) and determine whether the verse''s command to obey requires them to authorize armed action or merely implies they may.',
    'If the state-authority requirement is foundational and Qur''anically grounded, the reading is robust; if it developed for institutional consolidation, it is vulnerable to challenge from readings that claim fard ''ayn (individual obligation) in contexts where state authority is absent or apostate. Classification consequence: if contingent, the reading''s exclusion of revolutionary interpreters weakens from structural foreclosure to contextual institutional dominance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_authority_legitimacy_basis, empirical, 'Whether state-authority requirement is Qur''anic principle or jurisprudential development.').

omega_variable(
    proportionality_threshold_contestation,
    'What constitutes ''proportional response'' in this reading, and how is it adjudicated when Muslim communities face ongoing occupation or structural aggression? Is the proportionality gate stable or does it collapse under sustained asymmetric conflict?',
    'Analysis of fatwa literature and jurisprudential reasoning in 20th-21st century conflicts (Israel-Palestine, Kashmir, Myanmar Rohingya). Do established authorities maintain proportionality constraints, adjust them, or acknowledge their inoperability under conditions of protracted asymmetric warfare? Do they specify when response becomes disproportionate?',
    'If the proportionality constraint remains stable and authoritatively applied, the reading''s non-combatant immunity protection is real and enforced; if it collapses in high-conflict contexts, the constraint is context-dependent and may function more as a ''framing device'' than an operative limit. This affects the resistance score: high resistance suggests the constraint''s limits are real; if authorities consistently expand proportionality thresholds in practice, theater_ratio should rise and the constraint should reclassify toward snare territory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(proportionality_threshold_contestation, empirical, 'Whether proportionality threshold remains operative or collapses under sustained conflict.').

omega_variable(
    non_combatant_immunity_scope_ambiguity,
    'Does ''non-combatant'' mean (a) civilians not directly engaged in military action, (b) civilians not materially supporting military action, or (c) civilians not ideologically supporting aggression against Muslims? Which interpretation this reading adopts determines whether non-Muslims engaged in civil administration, logistics, or civic support are protected or targetable.',
    'Textual analysis of foundational jurisprudential texts (Muwatta, Hedaya, Umdat al-Salik) and contemporary fatwa rulings on legitimate targeting. Do authorities distinguish between civilian government officials, civilian military contractors, civilians working in defense industry, and ordinary civilians? Are consensus definitions stable or evolving?',
    'A narrow reading (only those directly wielding weapons) maximizes non-combatant immunity and beneficiaries outside the victim set. A broad reading (anyone supporting resistance to Islamic authority) erodes immunity and expands the victim set toward all non-Muslims in aggressors'' societies. Classification consequence: if (c) interpretation prevails, the constraint''s beneficiary structure (non-Muslims outside aggression as protected) collapses and extractiveness rises toward snare territory.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(non_combatant_immunity_scope_ambiguity, conceptual, 'Definition scope of ''non-combatant'' determines extent of protected populations.').

omega_variable(
    reading_kernel_relationship_contested,
    'Is the Qur''anic corpus sufficient to determine a single correct reading of jihad, or does the corpus itself contain incompatible claims that permit multiple readings with equal fidelity to the text? If the latter, on what grounds is this reading privileged over its siblings?',
    'Qur''anic textual analysis: catalog verses commanding inner struggle (22:78, 29:69), permitting defense (22:39-40, 2:190), describing expansion of Islamic authority (9:29, 9:73), and describing individual obligation (9:111). Determine whether these can be synthesized into a single hierarchy or whether they describe genuinely different contexts/obligations. Interview jurisprudential scholars on their selection criteria.',
    'If the corpus is truly indeterminate and this reading is privileged by institutional power rather than textual fidelity, the reading is a commitment-system winner but not a theological necessity. This affects the status of sibling readings: they would be equally defensible alternatives, not errors or heresies. Classification consequence: if the reading is contingent on institutional dominance rather than textual determination, the constraint''s persistence is more fragile and its theater_ratio should be higher (institutional maintenance replacing lost textual authority).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_relationship_contested, conceptual, 'Whether Qur''anic corpus determines this reading uniquely or permits multiple defensible readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__defensive_spiritual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(jiha_tr_t0, observed).
narrative_ontology:measurement(jiha_tr_t8, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 8, 0.09).
narrative_ontology:measurement_basis(jiha_tr_t8, observed).
narrative_ontology:measurement(jiha_tr_t16, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 16, 0.11).
narrative_ontology:measurement_basis(jiha_tr_t16, observed).
narrative_ontology:measurement(jiha_tr_t24, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 24, 0.12).
narrative_ontology:measurement_basis(jiha_tr_t24, observed).
narrative_ontology:measurement(jiha_tr_t32, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 32, 0.12).
narrative_ontology:measurement_basis(jiha_tr_t32, observed).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__defensive_spiritual_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement_basis(jiha_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement_basis(jiha_be_t0, observed).
narrative_ontology:measurement(jiha_be_t8, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 8, 0.29).
narrative_ontology:measurement_basis(jiha_be_t8, observed).
narrative_ontology:measurement(jiha_be_t16, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 16, 0.31).
narrative_ontology:measurement_basis(jiha_be_t16, observed).
narrative_ontology:measurement(jiha_be_t24, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 24, 0.32).
narrative_ontology:measurement_basis(jiha_be_t24, observed).
narrative_ontology:measurement(jiha_be_t32, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 32, 0.32).
narrative_ontology:measurement_basis(jiha_be_t32, observed).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__defensive_spiritual_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement_basis(jiha_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(jiha_su_t0, observed).
narrative_ontology:measurement(jiha_su_t8, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 8, 0.24).
narrative_ontology:measurement_basis(jiha_su_t8, observed).
narrative_ontology:measurement(jiha_su_t16, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 16, 0.26).
narrative_ontology:measurement_basis(jiha_su_t16, observed).
narrative_ontology:measurement(jiha_su_t24, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 24, 0.27).
narrative_ontology:measurement_basis(jiha_su_t24, observed).
narrative_ontology:measurement(jiha_su_t32, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 32, 0.28).
narrative_ontology:measurement_basis(jiha_su_t32, observed).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__defensive_spiritual_reading, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(jiha_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__defensive_spiritual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(jihad_quranic_corpus__defensive_spiritual_reading, 0.14).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__expansionist_legalist_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__defensive_spiritual_reading, jihad_quranic_corpus__revolutionary_vanguard_reading).

% DUAL FORMULATION NOTE:
% This constraint is part of the jihad_quranic_corpus family decomposed into three structurally distinct readings. The kernel is the standing Qur'anic teaching on jihad; each reading instantiates a different constraint because each produces a different victim set, different authority structure, and different scope of legitimate action. All three are derived from the same corpus but have different ε values and different types. The defensive_spiritual_reading has low extractiveness (0.32) and coordinates genuine spiritual meaning with state authority; the expansionist_legalist_reading has medium extractiveness and includes governance-establishment obligation; the revolutionary_vanguard_reading has high extractiveness and permits individual emergency authority. These are not the same constraint viewed from different angles — they are three separate constraints linked by their common kernel. Family links: defensive_spiritual -> expansionist_legalist (influences), defensive_spiritual -> revolutionary_vanguard (forecloses).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(jihad_quranic_corpus__defensive_spiritual_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
