% ============================================================================
% CONSTRAINT STORY: jihad_quranic_corpus__revolutionary_vanguard_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_jihad_quranic_corpus__revolutionary_vanguard_reading, []).

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
 *   constraint_id: jihad_quranic_corpus__revolutionary_vanguard_reading
 *   human_readable: Jihad as Fard 'Ayn Against Apostate Rulers and Occupiers (Revolutionary Vanguard Reading)
 *   domain: religious/political/legal
 *
 * SUMMARY:
 *   The revolutionary vanguard reading collapses the classical
 *   jurisprudential requirement that a legitimate imam or state authority
 *   declare and organize jihad. Instead, individual Muslims are held to be
 *   immediately and personally obligated to take up arms the moment a ruler
 *   is judged apostate (through takfir) or territory is judged occupied — no
 *   waiting for authorization, no adversarial process for the accused, and no
 *   independent body empowered to review the ruling. The doctrine
 *   additionally licenses treating civilians connected to the accused ruler
 *   or occupying power as complicit through association, dissolving the
 *   non-combatant immunity that both the defensive_spiritual_reading and the
 *   classical strand of the expansionist_legalist_reading preserve. The
 *   claimed type (tangled_rope) reflects a genuine coordination function —
 *   mobilizing dispersed actors without a centralized authority they consider
 *   compromised — layered onto asymmetric extraction from accused rulers,
 *   civilians reclassified as combatants, delegitimized jurists, and minority
 *   populations, sustained by active enforcement (assassination, insurgent
 *   campaigns, coercive recruitment).
 *
 * KEY AGENTS:
 *   - vanguard_ideologues: doctrinal authors and administrators of the emergency jurisprudence (organized/identity_locked) — set the takfir standard
 *   - takfiri_commanders: field operationalizers who convert doctrine into campaigns (organized/identity_locked) — capture territorial and organizational gains
 *   - accused_apostate_rulers: primary institutional targets stripped of appeal (institutional/trapped) — bear assassination and insurgency risk
 *   - muslim_civilians_under_collective_guilt: reclassified non-combatants (powerless/trapped) — bear violence without procedural standing
 *   - comparative_religious_law_scholars: analytical observers tracing the doctrine's genealogy and divergence from classical jurisprudence (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.81).
domain_priors:suppression_score(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.87).
domain_priors:theater_ratio(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, extractiveness, 0.81).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0.87).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(jihad_quranic_corpus__revolutionary_vanguard_reading, resistance, 0.88).

% --- Constraint claim ---
narrative_ontology:constraint_claim(jihad_quranic_corpus__revolutionary_vanguard_reading, tangled_rope).
narrative_ontology:human_readable(jihad_quranic_corpus__revolutionary_vanguard_reading, "Jihad as Fard 'Ayn Against Apostate Rulers and Occupiers (Revolutionary Vanguard Reading)").
narrative_ontology:topic_domain(jihad_quranic_corpus__revolutionary_vanguard_reading, "religious/political/legal").

domain_priors:requires_active_enforcement(jihad_quranic_corpus__revolutionary_vanguard_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(jihad_quranic_corpus__revolutionary_vanguard_reading, '4df4f45d-7623-41b8-8059-662c03c20999').
narrative_ontology:cs_kernel_codification('4df4f45d-7623-41b8-8059-662c03c20999', distributed).
narrative_ontology:cs_authority_grounding('4df4f45d-7623-41b8-8059-662c03c20999', extraction).
narrative_ontology:cs_interpretation_layer_present('4df4f45d-7623-41b8-8059-662c03c20999').
narrative_ontology:cs_reading_relation('4df4f45d-7623-41b8-8059-662c03c20999', jihad_quranic_corpus__defensive_spiritual_reading, forecloses).
narrative_ontology:cs_reading_relation('4df4f45d-7623-41b8-8059-662c03c20999', jihad_quranic_corpus__expansionist_legalist_reading, influences).
narrative_ontology:cs_axiom('4df4f45d-7623-41b8-8059-662c03c20999', foundational, individual_duty_overrides_state_authorization).
narrative_ontology:cs_axiom_status(individual_duty_overrides_state_authorization, holdable).
narrative_ontology:cs_axiom_grounding('4df4f45d-7623-41b8-8059-662c03c20999', individual_duty_overrides_state_authorization, conventional).
narrative_ontology:cs_axiom('4df4f45d-7623-41b8-8059-662c03c20999', foundational, necessity_doctrine_suspends_procedural_safeguards).
narrative_ontology:cs_axiom_status(necessity_doctrine_suspends_procedural_safeguards, holdable).
narrative_ontology:cs_axiom_grounding('4df4f45d-7623-41b8-8059-662c03c20999', necessity_doctrine_suspends_procedural_safeguards, instrumental).
narrative_ontology:cs_axiom('4df4f45d-7623-41b8-8059-662c03c20999', secondary, associational_complicity_dissolves_noncombatant_immunity).
narrative_ontology:cs_axiom_status(associational_complicity_dissolves_noncombatant_immunity, holdable).
narrative_ontology:cs_axiom_grounding('4df4f45d-7623-41b8-8059-662c03c20999', associational_complicity_dissolves_noncombatant_immunity, conventional).
narrative_ontology:cs_reference_frame('4df4f45d-7623-41b8-8059-662c03c20999', classical_imam_authorized_jihad).
narrative_ontology:cs_drift_state('4df4f45d-7623-41b8-8059-662c03c20999', post_20th_century_revolutionary_islamist_theory, gap(authority_erosion, severe, false)).
narrative_ontology:cs_created_at('4df4f45d-7623-41b8-8059-662c03c20999', '').
narrative_ontology:cs_kernel_id(jihad_quranic_corpus__revolutionary_vanguard_reading, jihad_quranic_corpus).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_ideologues).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, takfiri_commanders).
narrative_ontology:constraint_beneficiary(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_insurgent_factions).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, accused_apostate_rulers).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_under_collective_guilt).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, occupied_territory_populations).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jurists_delegitimized).
narrative_ontology:constraint_victim(jihad_quranic_corpus__revolutionary_vanguard_reading, religious_minorities_in_contested_zones).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, individual_religious_duty_supersedes_state_monopoly).
narrative_ontology:constraint_vindicates(jihad_quranic_corpus__revolutionary_vanguard_reading, emergency_doctrine_permits_bypass_of_classical_safeguards).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Author and disseminate the doctrine that jihad becomes fard 'ayn (individual obligation) the moment a Muslim territory is deemed occupied or its ruler deemed apostate (takfir), collapsing the classical requirement of imam authorization. They issue fatwas, train cadres, and administer the emergency jurisprudence that licenses bypassing state religious authority. Their standing and material support depend on the doctrine's continued circulation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, vanguard_ideologues, agenda_setter,
    organized, generational, identity_locked, global).

% Field commanders who operationalize the doctrine, declaring specific rulers, security forces, and sometimes entire populations complicit and therefore lawful targets. They recruit fighters using the fard 'ayn framing (an individual duty no one may excuse himself from) and gain territorial control, resources, and legitimacy claims through the campaigns the doctrine authorizes.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, takfiri_commanders, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(jihad_quranic_corpus__revolutionary_vanguard_reading, takfiri_commanders, agenda_setter).

% Competing armed groups adopt the same emergency-jurisprudence logic to challenge both incumbent regimes and each other, using takfir as a currency of legitimacy in intra-jihadist competition. They benefit from the doctrine's decentralization because it removes any single gatekeeper who could rule their claims illegitimate.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, rival_insurgent_factions, beneficiary,
    organized, biographical, mobile, regional).

% Heads of state and security officials in majority-Muslim countries who are declared apostate for governing by non-shari'a law, allying with non-Muslim powers, or failing to implement the vanguard's preferred order. Once takfir is pronounced, they face assassination attempts, insurgency, and the collapse of any claim to religious legitimacy they might otherwise hold. They cannot appeal to classical jurisprudential process because the doctrine explicitly bypasses it.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, accused_apostate_rulers, payer,
    institutional, immediate, trapped, national).

% Ordinary residents, security personnel's families, taxpayers, voters, and civil servants in territories governed by the accused ruler are reclassified as complicit through association, employment, or simple presence, dissolving the classical non-combatant immunity. They bear bombings, extrajudicial killings, and displacement without having had any say in the takfir ruling that redefined their status.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, muslim_civilians_under_collective_guilt, payer,
    powerless, immediate, trapped, regional).

% Civilians living under what the doctrine designates foreign occupation become the terrain and sometimes the currency of the campaign: their suffering is cited to justify escalation, but the doctrine's emergency logic also authorizes strikes that treat their presence near occupying forces as acceptable collateral or as evidence of collaboration.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, occupied_territory_populations, payer,
    powerless, immediate, trapped, regional).

% Established scholars and state-affiliated religious institutions who hold that jihad requires an imam's authorization, proportionality review, and formal declaration find their entire jurisprudential authority declared irrelevant or complicit. Their fatwas against the vanguard reading are dismissed as products of a corrupted, state-captured clergy, stripping them of the audience and authority they previously held.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, classical_jurists_delegitimized, payer,
    institutional, generational, constrained, global).

% Non-Muslim and minority-sect populations living in territories where the vanguard doctrine is operative face targeted violence justified by expansive definitions of occupation and complicity that exceed even the expansionist classical framework's conditions for engagement, since the emergency doctrine removes the procedural steps (invitation, warning, proportionality assessment) that would have applied to them.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, religious_minorities_in_contested_zones, payer,
    powerless, immediate, trapped, regional).

% The governments targeted by this reading have no seat in the doctrinal dispute — the takfir ruling that delegitimizes them is issued unilaterally by non-state religious authorities they do not recognize. Their only available responses are counterinsurgency and competing state-sanctioned religious counter-fatwas, neither of which engages the doctrine on its own jurisprudential terms.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, state_security_apparatuses, excluded,
    institutional, immediate, constrained, national).

% Study how this reading diverges from classical and modern mainstream jurisprudence on jihad, tracing its genealogy through 20th-century revolutionary Islamist theorists and its adoption by specific insurgent and terrorist organizations. They document the doctrine's effects without holding power to alter its circulation.
narrative_ontology:constraint_stakeholder(jihad_quranic_corpus__revolutionary_vanguard_reading, comparative_religious_law_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(jihad_quranic_corpus__revolutionary_vanguard_reading, diffuse).
narrative_ontology:fixing_cost_class(jihad_quranic_corpus__revolutionary_vanguard_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a mobilization framework that allows dispersed individuals and small cells to act without waiting for centralized state or clerical authorization, solving a genuine coordination problem for actors who believe existing Muslim states and religious establishments are themselves compromised or captured.
% TRANSFER_FUNCTION: Moves lives, territory, political legitimacy, and material resources away from incumbent rulers, their security forces, ordinary civilians in contested zones, and religious minorities, toward the vanguard organizations and commanders who administer the takfir rulings and command the resulting campaigns.
% ABSENT_VOICES: Accused rulers, the civilian populations reclassified as complicit, classical jurists whose authority is declared void, and religious minorities in contested zones have no procedural standing in the takfir determination that redefines their status — the ruling is issued by the vanguard itself with no adversarial process, appeal, or independent adjudication.
% DISAPPEARANCE_RATIONALE: If this doctrine's mobilizing authority vanished overnight, dispersed cells would lose their jurisprudential license to act without state or established clerical sanction; recruitment premised on fard 'ayn urgency would collapse, takfir-driven insurgencies would lose a key legitimation mechanism, and conflicts currently organized around this framework would have to recompose around other authority structures (state authority, classical jurisprudential consensus, or purely political/nationalist framings).
% FOUNDING_PROBLEM: 20th-century revolutionary Islamist theorists confronted what they saw as apostate postcolonial regimes and foreign military occupation, and classical jurisprudence's requirement of imam authorization for jihad left no legitimate path to armed resistance when the recognized authority was itself the target.
% FOUNDING_PROBLEM_CORROBORATION: Vanguard ideologues and commanders attest the founding problem (illegitimate rulers, foreign occupation) remains live and justifies continued mobilization. Comparative religious law scholars, mainstream jurists, and the targeted states attest from outside the benefiting parties that the doctrine has become a self-perpetuating legitimation mechanism for organizational survival and rivalry among insurgent factions, disconnected from case-by-case assessment of whether occupation or apostasy actually obtains — and that its collective-guilt reasoning exceeds even the classical expansionist framework's own procedural constraints.
narrative_ontology:disappearance_verdict(jihad_quranic_corpus__revolutionary_vanguard_reading, world_rearranges).
narrative_ontology:founding_problem_status(jihad_quranic_corpus__revolutionary_vanguard_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(jihad_quranic_corpus__revolutionary_vanguard_reading, 'none', 1).
narrative_ontology:epsilon_provenance(jihad_quranic_corpus__revolutionary_vanguard_reading, 0.81, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(jihad_quranic_corpus__revolutionary_vanguard_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(jihad_quranic_corpus__revolutionary_vanguard_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction rises over the interval (0.55 to 0.81) tracking the doctrine's spread from theoretical writings to operational insurgent practice across multiple organizations and regions. Suppression is high throughout and rises further (0.60 to 0.87) because the doctrine's persistence depends on active coercion — recruitment pressure framed as inescapable individual duty, punishment of dissenting voices within the movement, and violence against those who reject the takfir ruling. Theater ratio rises moderately (0.20 to 0.42) as competing factions increasingly invoke the doctrine performatively to claim legitimacy in intra-jihadist rivalry rather than in direct response to a specific occupation or apostasy event, a driver documented in the rival_insurgent_factions stakeholder entry. Accessibility collapse (0.62) is more moderate than a genuine mountain because alternative religious-legal framings (the two sibling readings) remain actively available and contested, not extinguished. Resistance is very high (0.88) reflecting sustained opposition from targeted states, mainstream clerical establishments, and much of the civilian population.
 *
 * DIRECTIONALITY LOGIC:
 *   Vanguard ideologues and takfiri commanders sit near the full-beneficiary end: they author and administer the ruling, and organizational survival, resources, and legitimacy accrue to them from its operation. Accused rulers, reclassified civilians, and religious minorities sit near the full-target end: trapped exit options, no procedural voice in the ruling that redefines their status, and direct exposure to violence justified by the doctrine. Classical jurists occupy an unusual position — they are not physically targeted in most cases but suffer a genuine extraction of authority and audience, which is why they are listed as payers despite institutional power; their exit option is constrained rather than trapped because they retain platforms, just diminished ones.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — apostate postcolonial rulers and foreign occupation with no jurisprudentially sanctioned path to resistance — was historically live for specific movements at specific moments. But the doctrine's persistence and expansion into intra-jihadist rivalry (documented via rival_insurgent_factions) and its increasingly performative invocation (rising theater_ratio) indicate the mandate has substantially outlived case-specific justification: takfir is issued in circumstances far removed from clear-cut foreign occupation, and the doctrine now functions as a general-purpose legitimation tool for organizational survival. Classifying this as tangled_rope rather than pure snare preserves the genuine (if contested) coordination function the doctrine originally solved for actors facing a real authorization vacuum, while the required beneficiary/victim/enforcement structure captures the asymmetric extraction that has since dominated its operation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest_jihad_corpus,
    'The jihad_quranic_corpus kernel supports at least three structurally distinct readings — defensive_spiritual_reading, expansionist_legalist_reading, and this revolutionary_vanguard_reading — each with different authority structures, victim sets, and procedural safeguards. Which reading, if any, represents the dominant or normative interpretation within contemporary Islamic jurisprudence, and does that dominance status affect how this reading''s extraction should be weighted against the others?',
    'Comparative survey of contemporary fatwa councils, state-affiliated religious authorities, and academic Islamic legal scholarship across major schools (Sunni madhhabs, Shi''a jurisprudence) to establish which reading commands broader scholarly and institutional assent, cross-referenced against the actual operational prevalence of groups adopting the vanguard reading.',
    'If the vanguard reading is a marginal minority position rejected by the overwhelming majority of Islamic legal scholarship, that strengthens the case that its extraction operates through doctrinal capture of a small movement rather than broad religious consensus, sharpening the payer/beneficiary asymmetry already authored. If it commands wider tacit sympathy in specific contexts of occupation, the coordination function claim strengthens correspondingly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_jihad_corpus, conceptual, 'Whether this reading is a minority doctrinal position or holds broader contemporary legitimacy relative to its siblings.').

omega_variable(
    emergency_doctrine_genuine_vs_pretextual,
    'Is the emergency jurisprudence that overrides classical safeguards (imam authorization, proportionality review, non-combatant immunity) a genuine application of recognized Islamic legal principles of necessity (darura), or a post-hoc doctrinal construction designed to license predetermined violence?',
    'Textual and historical analysis comparing the vanguard reading''s invocation of necessity doctrine against classical usul al-fiqh treatments of darura, and empirical tracing of whether takfir rulings precede or follow operational decisions to attack specific targets.',
    'If emergency doctrine is applied as genuine necessity reasoning consistent with classical method, the coordination function is stronger than authored. If takfir rulings are consistently issued to retroactively justify attacks already planned, the tangled_rope classification understates extraction and a snare classification would be more accurate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(emergency_doctrine_genuine_vs_pretextual, empirical, 'Whether the doctrine''s necessity reasoning is methodologically genuine or pretextual.').

omega_variable(
    collective_guilt_scope_ambiguity,
    'How far does the collective-guilt reasoning that reclassifies civilians as complicit actually extend in practice — is it applied narrowly (direct security personnel, active collaborators) or broadly (taxpayers, voters, civil servants, families)?',
    'Documentation and case analysis of specific attacks attributed to organizations operating under this doctrine, categorizing the stated justification for each target''s inclusion in the complicit class.',
    'A narrow application would moderate the accessibility_collapse and victim-set breadth authored here; a broad application, which the historical record for several vanguard-influenced organizations suggests, confirms the wide civilian victim set as authored and would support an even higher extractiveness score.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_guilt_scope_ambiguity, empirical, 'The actual operational breadth of collective guilt reasoning in target selection.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(jihad_quranic_corpus__revolutionary_vanguard_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jiha_tr_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(jiha_tr_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 10, 0.26).
narrative_ontology:measurement(jiha_tr_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 20, 0.32).
narrative_ontology:measurement(jiha_tr_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 30, 0.36).
narrative_ontology:measurement(jiha_tr_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 40, 0.39).
narrative_ontology:measurement(jiha_tr_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, theater_ratio, 50, 0.42).

% Extraction over time
narrative_ontology:measurement(jiha_be_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(jiha_be_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 10, 0.63).
narrative_ontology:measurement(jiha_be_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 20, 0.7).
narrative_ontology:measurement(jiha_be_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 30, 0.75).
narrative_ontology:measurement(jiha_be_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 40, 0.79).
narrative_ontology:measurement(jiha_be_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, base_extractiveness, 50, 0.81).

% Suppression requirement over time
narrative_ontology:measurement(jiha_su_t0, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(jiha_su_t10, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 10, 0.68).
narrative_ontology:measurement(jiha_su_t20, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 20, 0.75).
narrative_ontology:measurement(jiha_su_t30, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 30, 0.8).
narrative_ontology:measurement(jiha_su_t40, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 40, 0.84).
narrative_ontology:measurement(jiha_su_t50, jihad_quranic_corpus__revolutionary_vanguard_reading, suppression_requirement, 50, 0.87).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(jihad_quranic_corpus__revolutionary_vanguard_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, defensive_spiritual_reading).
narrative_ontology:affects_constraint(jihad_quranic_corpus__revolutionary_vanguard_reading, expansionist_legalist_reading).

% DUAL FORMULATION NOTE:
% This story is one of three linked readings of the jihad_quranic_corpus kernel. The defensive_spiritual_reading and expansionist_legalist_reading are siblings, not variants of this constraint — each carries its own ε, its own beneficiary/victim structure, and its own claimed type. This reading forecloses the defensive_spiritual_reading's core premise (that jihad is primarily internal/spiritual and armed response is strictly defensive and proportionality-bound) because the vanguard reading's fard 'ayn mobilization against declared apostates is incompatible with a framework limiting jihad to proportionate defense against external aggression — a single jurisprudential framework cannot coherently hold both. It influences (without foreclosing) the expansionist_legalist_reading by creating downstream pressure: the vanguard reading's decentralization and bypass of imam authorization erodes the legitimacy conditions the expansionist reading depends on (state/imam sanction), without making the expansionist reading itself impossible to hold in other frameworks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
