% ============================================================================
% CONSTRAINT STORY: quran_9_5_scope__contextual_defensive
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_9_5_scope__contextual_defensive, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: quran_9_5_scope__contextual_defensive
 *   human_readable: Qur'an 9:5 Contextual Defensive Interpretation
 *   domain: religious/political_theology
 *
 * SUMMARY:
 *   Qur'an 9:5 is one of the most contested verses in Islamic jurisprudence.
 *   The contextual-defensive reading interprets it as a historically-bounded
 *   response to 7th-century Medinan treaty violations by polytheist tribes,
 *   not a universal abrogation of peaceful coexistence norms. Under this
 *   reading, the verse authorizes DEFENSIVE warfare against treaty violators
 *   only, leaving all peaceful verses intact and all non-violating polities
 *   protected. This reading benefits integrationist Muslim-majority states
 *   seeking pluralistic governance frameworks and scholars advocating
 *   peaceful coexistence; it constrains literalist-maximalist scholars whose
 *   advocacy depends on offensive-jihad framing. The claim/metric gap is
 *   deliberate: the reading is CLAIMED as tangled_rope (coordination of
 *   defensive doctrine + treaty obligations + institutional enforcement)
 *   while the authored metrics describe low-to-moderate extraction with
 *   modest suppression—reflecting the reading's structural position as
 *   enabling rather than coercive.
 *
 * KEY AGENTS:
 *   - Integrationist Muslim-majority states (beneficiaries, institutional power)
 *   - Coexistence norm adherents (beneficiaries, organized, global reach)
 *   - Treaty violating actors (victims/targets, powerful, bounded scope)
 *   - Peaceful non-Muslim polities (dual-positioned: beneficiaries + diffuse cost-bearers)
 *   - Literalist-maximalist scholars (payers of lost institutional authority)
 *   - Medieval Islamic jurists (analytical observers, corroborators)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_9_5_scope__contextual_defensive, 0.15).
domain_priors:suppression_score(quran_9_5_scope__contextual_defensive, 0.22).
domain_priors:theater_ratio(quran_9_5_scope__contextual_defensive, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, extractiveness, 0.15).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(quran_9_5_scope__contextual_defensive, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_9_5_scope__contextual_defensive, tangled_rope).
narrative_ontology:human_readable(quran_9_5_scope__contextual_defensive, "Qur'an 9:5 Contextual Defensive Interpretation").
narrative_ontology:topic_domain(quran_9_5_scope__contextual_defensive, "religious/political_theology").

domain_priors:requires_active_enforcement(quran_9_5_scope__contextual_defensive).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_9_5_scope__contextual_defensive, 'f6fccaac-2846-442a-9af2-3ac412efef14').
narrative_ontology:cs_kernel_codification('f6fccaac-2846-442a-9af2-3ac412efef14', fixed_text).
narrative_ontology:cs_authority_grounding('f6fccaac-2846-442a-9af2-3ac412efef14', lineage).
narrative_ontology:cs_interpretation_layer_present('f6fccaac-2846-442a-9af2-3ac412efef14').
narrative_ontology:cs_reading_relation('f6fccaac-2846-442a-9af2-3ac412efef14', quran_9_5_scope__abrogating_universal, coexists_with).
narrative_ontology:cs_reading_relation('f6fccaac-2846-442a-9af2-3ac412efef14', quran_9_5_scope__progressive_synthesis, influences).
narrative_ontology:cs_axiom('f6fccaac-2846-442a-9af2-3ac412efef14', foundational, context_specificity_not_universal_abrogate).
narrative_ontology:cs_axiom_status(context_specificity_not_universal_abrogate, holdable).
narrative_ontology:cs_axiom_grounding('f6fccaac-2846-442a-9af2-3ac412efef14', context_specificity_not_universal_abrogate, empirically_contingent).
narrative_ontology:cs_axiom('f6fccaac-2846-442a-9af2-3ac412efef14', foundational, treaty_violation_precondition_for_warfare).
narrative_ontology:cs_axiom_status(treaty_violation_precondition_for_warfare, holdable).
narrative_ontology:cs_axiom_grounding('f6fccaac-2846-442a-9af2-3ac412efef14', treaty_violation_precondition_for_warfare, deontological).
narrative_ontology:cs_reference_frame('f6fccaac-2846-442a-9af2-3ac412efef14', contextual_medinan_defense_doctrine).
narrative_ontology:cs_drift_state('f6fccaac-2846-442a-9af2-3ac412efef14', contemporary_pluralistic_governance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('f6fccaac-2846-442a-9af2-3ac412efef14', '').
narrative_ontology:cs_kernel_id(quran_9_5_scope__contextual_defensive, quran_9_5_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, coexistence_norm_adherents).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, treaty_violating_polytheist_actors).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, peaceful_non_muslim_polities).
narrative_ontology:constraint_beneficiary(quran_9_5_scope__contextual_defensive, interfaith_peacebuilding_organizations).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, peaceful_non_muslim_polities).
narrative_ontology:constraint_victim(quran_9_5_scope__contextual_defensive, literalist_maximalist_scholars).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% States that adopt the contextual-defensive reading benefit from a jurisprudential framework that permits peaceful coexistence with non-Muslim polities and minorities. This reading legitimates pluralistic governance, treaty enforcement across religious boundaries, and defensive-only military doctrine. The state can claim Islamic legal grounding for integration policies without ceding religious authority to maximalist interpretations. Exit: the state can switch to progressivist readings or abandon Islamic jurisprudential grounding entirely (arbitrage).
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states, beneficiary,
    institutional, generational, arbitrage, national).

% Muslim scholars, interfaith organizations, and civil-society groups that advocate for peaceful pluralism gain jurisprudential legitimacy and interpretive authority when the contextual-defensive reading is institutionalized in education, legal systems, and fatwa bodies. Their advocacy for coexistence becomes anchored in canonical text rather than positioned as accommodation to external pressure. Exit: they can mobilize support for progressivist or other readings (mobile).
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, coexistence_norm_adherents, beneficiary,
    organized, generational, mobile, global).

% Groups historically cast as 7th-century treaty-breaking polytheist tribes (and in contemporary application, actors who commit treaty violations or aggression against Muslim polities) are the specific targets of the defensive-military permission. They bear the consequence of the constraint's enforcement: defensive warfare is permitted against them, but ONLY when they initiate aggression or breach prior compacts. Their exit option is treaty compliance and non-aggression—constrained because violation triggers the permission.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, treaty_violating_polytheist_actors, payer,
    powerful, biographical, constrained, regional).

% Non-Muslim states and communities that maintain treaties and do not initiate aggression are protected under the contextual-defensive reading: they are NOT targets of the constraint because the reading requires prior violation as a condition. They benefit from legal clarity that peaceful coexistence is mandated and from the institutional weight behind the reading. However, they also bear the ongoing structural cost of remaining in a framework where war IS permitted against treaty violators, creating deterrent pressure and strategic vulnerability. Exit: they can breach treaties (accepting warfare consequence), escalate armament (arbitrage toward power symmetry), or maintain peace (continue as protected).
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, peaceful_non_muslim_polities, beneficiary,
    powerful, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(quran_9_5_scope__contextual_defensive, peaceful_non_muslim_polities, payer).

% Scholars and movements that advocate the abrogating-universal or broader-offensive interpretations experience institutional and political pressure when the contextual-defensive reading is adopted by state authorities, educational systems, and fatwa bodies. Their interpretive authority is contested; their preferred reading is sidelined in official channels, though it remains live in non-state networks. The cost they bear is diminished institutional influence over Islamic jurisprudence in pluralistic contexts. Exit: they are identity-locked because intellectual/professional identity is fused with literalist framing; exiting means professional death in traditionalist institutions. They can maintain counter-networks but cannot easily exit the identity.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, literalist_maximalist_scholars, payer,
    organized, generational, identity_locked, global).

% The historical scholarly tradition (Ibn Abbas, al-Tabari, al-Zamakhshari, Ibn Kathir, and others) represents an analytical seat that attests to the contextual reading's deep roots in classical tafsir (exegesis). Their corpus provides external corroboration for the reading's foundational claim. They are not parties to the constraint but observers whose testimony is invoked to validate readings.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, medieval_islamic_jurists, observer,
    analytical, civilizational, analytical, universal).

% NGOs and civil-society bodies working on interfaith dialogue and conflict reduction benefit from the contextual-defensive reading because it provides Islamic jurisprudential support for peacebuilding rhetoric and norm-setting. They can cite this reading in advocacy for non-violence, contributing to institutional coordination around coexistence norms. Exit: they can adopt other readings or abandon Islamic legal grounding (mobile).
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, interfaith_peacebuilding_organizations, beneficiary,
    moderate, biographical, mobile, global).

% Movements advocating the progressive-synthesis reading (temporal-ethical trajectory interpretation, not eternal command) are not directly in the conversation when the contextual-defensive reading is institutionalized. They would argue for deeper hermeneutical revisionism but are excluded from or marginalized in official jurisprudential channels that adopt the contextual reading as the settled norm. They perceive the contextual reading as not going far enough but cannot mobilize state backing for progressivist doctrine.
narrative_ontology:constraint_stakeholder(quran_9_5_scope__contextual_defensive, progressive_reinterpretive_movements, excluded,
    moderate, biographical, constrained, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_9_5_scope__contextual_defensive, integrationist_muslim_majority_states).
narrative_ontology:fixing_cost_class(quran_9_5_scope__contextual_defensive, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates defensive military action with treaty obligation norms: the reading establishes that Islam permits warfare ONLY against treaty violators and aggressors, which aligns Muslim-majority states' security interests with international legal norms of jus in bello and treaty pacta sunt servanda. It solves the coordination problem of how to ground Islamic law in both security doctrine and pluralistic governance, preventing unilateral reinterpretation of religious texts as warrant for unlimited warfare.
% TRANSFER_FUNCTION: Transfers interpretive authority from literalist-maximalist scholars to integrationist jurists and state authorities; redistributes legitimacy from offensive-jihad framings to defensive-law framings; moves the 9:5 verse from a standing authorization for conquest to a conditional response to breach. The constraint transfers institutional power (who gets to define Islamic law in state systems) from maximalist networks to pluralistic institutions. Economically, it transfers prestige and publication/teaching opportunities from one school of thought to another.
% ABSENT_VOICES: Literalist scholars advocating abrogating-universal or offensive-jihad readings are structurally absent from official channels when this reading dominates; they can be heard in non-state networks, social media, certain madrasas, and traditionalist circles but lack institutional megaphone. Maximalist political movements that depend on offensive-jihad framing are also excluded from the institutional conversation, though they remain live in non-state discourse and can command support in disaffected populations.
% DISAPPEARANCE_RATIONALE: If this constraint (the contextual-defensive reading's institutional adoption) disappeared and literalist-maximalist readings became dominant in Islamic jurisprudence and state law, pluralistic governance frameworks in Muslim-majority states would collapse or face severe delegitimation, treaty enforcement norms would weaken, and the conceptual ground for peaceful coexistence would shift. Conversely, if this reading is permanently institutionalized, competing interpretations persist in non-state spaces but cannot command state enforcement machinery, leaving maximalist movements to operate through social persuasion and non-state institutions only.
% FOUNDING_PROBLEM: 7th-century Medina required a legal and political framework that permitted defensive military response to polytheist tribes breaking treaties while maintaining a doctrine of peace for those honoring compacts. The founding problem is: how to ground Islamic law in security doctrine without making unlimited warfare the standing legal state?
% FOUNDING_PROBLEM_CORROBORATION: Medieval Islamic jurists (al-Tabari, Ibn Kathir, al-Zamakhshari, Ibn Abbas) attest the contextual reading from within the classical exegetical tradition, independent of modern pluralism pressures. Their tafsir works establish that 9:5 was understood contextually in relation to 7th-century treaty violations, not as a universal abrogate, within the first centuries of Islamic jurisprudence. Contemporary Muslim scholars, interfaith organizations, and secular historians of Islamic law outside the maximalist lobby also corroborate the historical accuracy of the 7th-century context and the reading's textual grounding. Literalist scholars dispute that the founding problem was defensive-only, claiming instead that offensive jihad was always the standing doctrine—but this dispute is ITSELF the kernel contest, not independent evidence. The corroboration comes from classical authorities and non-partisan historians, not from the beneficiaries' own arguments.
narrative_ontology:disappearance_verdict(quran_9_5_scope__contextual_defensive, world_rearranges).
narrative_ontology:founding_problem_status(quran_9_5_scope__contextual_defensive, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_9_5_scope__contextual_defensive, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku+stakeholder_backfill', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(quran_9_5_scope__contextual_defensive, 'none', 1).
narrative_ontology:epsilon_provenance(quran_9_5_scope__contextual_defensive, 0.15, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_9_5_scope__contextual_defensive_tests).
:- end_tests(quran_9_5_scope__contextual_defensive_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.15) because the contextual-defensive reading does not impose unilateral costs on peaceful actors—it is permissive (defensive war allowed) rather than coercive (mandatory submission). Suppression is modest (0.22) because the reading does not require silencing alternative interpretations; literalist readings persist in non-state networks, and the contextual reading gains institutional ground through scholarly persuasion and state adoption, not coercive suppression of rivals. Theater is minimal (0.08) because the reading's function is stable: it genuinely coordinates defensive doctrine with treaty norms without requiring performative maintenance. Accessibility of alternatives is moderate (0.45) because the literalist-maximalist readings remain intellectually accessible but are institutionally marginalized when the contextual reading is adopted by state authorities and major educational bodies. Resistance is HIGH (0.72) because literalist scholars and maximalist movements actively contest the contextual reading, mobilize counter-arguments from classical sources, and maintain alternative institutional channels (non-state networks, certain madrasas, social-media platforms). The measurement series show modest decline over time: extractiveness decreases slightly as institutional adoption deepens (beneficiary states consolidate the reading), while suppression declines as the reading becomes less contested (familiarity reduces friction). Projections show slight uptick in later periods as geopolitical contestation may revive maximalist framing.
 *
 * PERSPECTIVAL GAP:
 *   The BENEFICIARY SEAT (integrationist states, coexistence adherents) experiences this constraint as enabling coordination: it permits defensive war while protecting peaceful coexistence, aligning Islamic law with pluralistic governance. The VICTIM SEAT (treaty-violating actors, literalist scholars losing institutional authority) experiences it as constraining: their preferred framings are sidelined, their offensive-jihad readings lack state backing, and their interpretive authority is diminished. From the OBSERVER SEAT (medieval jurists, secular scholars), the constraint is analytically neutral—it is a historically-accurate reading of classical exegesis, neither inherently coercive nor liberatory. The engine will compute these three seats' classifications differently from the same structural data: beneficiaries see coordination (lower d, lower χ), victims see extraction (higher d, higher χ), observers sit near symmetric (d~0.5, χ moderate). This is not a failure of the constraint—it is exactly what per-seat classification measures.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from two axes: (1) structural position in the constraint's benefit/cost flow, and (2) exit options. Integrationist states and coexistence adherents are beneficiaries with MOBILE/ARBITRAGE exit (they can adopt this reading or switch to progressivist readings; institutional adoption is a choice, not coercion). This places them near d=0.0 (full beneficiary). Treaty-violating actors are TARGETS with CONSTRAINED exit (they can only exit by ceasing aggression/violations; the constraint's operation is contingent on their violation). This places them near d=1.0 (full target). Literalist-maximalist scholars are a secondary-payer group with IDENTITY_LOCKED exit (their intellectual/professional identity is fused with offensive-jihad framing; exiting the literalist framework means professional death in traditionalist institutions). This places them near d=0.75–0.85. Peaceful non-Muslim polities are structurally dual: beneficiaries (protected from war unless they violate) with ARBITRAGE exit (maintain treaties, avoid war; exit by breaching treaties, then face defensive response). This places them near d=0.4 (mildly beneficiary, with conditional exposure). The derived directionality map reflects these structural positions WITHOUT requiring overrides for the institutional and organized seats—the power atoms and exit options produce the right d values from the beneficiary/victim declarations alone.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is LIVE, not dead: the founding problem (7th-century defensive capability + treaty enforcement) persists in contemporary form (Muslim-majority states need legal grounds for defensive posture while maintaining pluralistic governance). The reading is NOT performing theater; it is actively solving the coordination problem it was built for. However, a mandatrophy signal would emerge if the constraint BECAME LIVE only as theater—if integrationist states adopted the contextual reading merely as PR cover while maximalist violence and offensive doctrine continued in practice, with the reading used selectively to deny culpability. The contemporaneous challenge (Daesh, Taliban-adjacent groups invoking offensive-jihad framings) creates the conditions for mandatrophy: if the reading becomes institutional window-dressing while maximalist actors command actual political/military power, the constraint would degrade into performance. Current measurement shows the constraint is performing function (low theater, stable coordination across states and scholars), but future projections carry uncertainty: if geopolitical shifts strengthen maximalist movements, the contextual reading could become decorative while losing operative force. This is captured in the omega variable below.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    contextual_scope_boundary,
    'Does the contextual reading truly limit the verse to 7th-century Medinan treaty violators, or does the reading admit principled extension to contemporary actors meeting the ''treaty violation'' criterion, thereby covertly universalizing the permission?',
    'Fatwa bodies and scholarly literature studying contemporary application of the contextual reading: does it restrict defense-permission ONLY to state-level treaty violations between signatories, or does it extend to non-state actors, asymmetric violations, and anticipatory claims of breach? A narrow application pattern supports the reading''s localizing claim; broad application suggests hidden universalization.',
    'If the reading admits covert universalization (any actor claiming a non-Muslim ''broke faith'' can invoke the permission), the constraint collapses toward the abrogating-universal reading, and ε rises substantially (0.15 → 0.55+). If application remains strictly bounded to state-treaty contexts, the reading is structurally distinct and ε remains low.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contextual_scope_boundary, empirical, 'Whether contextual-defensive boundaries hold in practice or admit hidden universalization.').

omega_variable(
    institutional_capture_risk,
    'Can integrationist states adopt the contextual-defensive reading institutionally while maximalist movements retain popular mobilizing power, creating a bifurcated jurisprudence where the reading becomes official cover for policies that contradict its mandate?',
    'Monitoring of state-fatwa consistency with actual military policy; tracking whether non-state maximalist movements gain recruits and resources despite institutional adoption of the contextual reading; measuring disjunction between state legal doctrine and actual doctrine-practice of armed groups operating under state tolerance or within state borders.',
    'If bifurcation occurs (state law = contextual-defensive; actual practice = maximalist offensive doctrine), the constraint degrades toward piton classification (theater increases, functional mandate atrophies, suppression of maximalist readings increases to maintain the cover story). The constraint would become mandatrophic, and ε measured at the enforcement level rises (theater ratio → 0.40+) while state-level doctrine stays low (institutional ε remains ~0.15). This is not a failure of the reading but a failure of institutional commitment to it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_capture_risk, empirical, 'Risk that the reading becomes institutional cover story while maximalist practice persists.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the contextual-defensive reading''s core premise (context-specificity, no universal abrogate) logically foreclose the abrogating-universal reading, or do the readings merely compete for institutional adoption?',
    'Textual analysis and internal consistency study: can a framework simultaneously hold (1) that 9:5 was context-specific and (2) that 9:5 abrogated all peaceful verses for all time? If the two claims are logically contradictory, the reading forecloses. If a framework can hold both (by distinguishing historical fact from normative consequence), they coexist.',
    'FORECLOSURE DETERMINATION shifts the reading_relations value from ''coexists_with'' to ''forecloses''. If foreclosure is confirmed, the contextual-defensive reading (through institutional adoption) logically eliminates the abrogating-universal framework for any party holding it. If coexistence is confirmed, the readings remain live options across different institutional seats even if one is institutionally dominant.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether the readings are logically contradictory (forecloses) or merely competing (coexists).').

omega_variable(
    beneficiary_circularity,
    'Are integrationist states genuine beneficiaries of this reading, or are they circular authors of it—states adopt the reading BECAUSE they want pluralistic governance, not because the reading independently licenses that governance?',
    'Historical analysis of pre-reading institutional adoption: did Muslim-majority states independently arrive at integrationist governance and then seek jurisprudential grounding, or did scholarly work on the contextual-defensive reading convince them to adopt pluralistic governance? Timing and causal order matter.',
    'If states are circular authors (adopting reading to justify pre-existing preferences), the reading is better classified as a beneficiary legitimation constraint than as a coordination constraint—ε might rise to 0.25–0.35 (extractive legitimacy work), and the reading becomes more snare-like (providing cover for state interests). If the reading independently persuaded states, it is genuinely coordinative.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_circularity, empirical, 'Whether integrationist states are beneficiaries or circular authors of the reading.').

omega_variable(
    abrogation_doctrine_ambiguity,
    'What does ''abrogate'' (nasikh) mean in classical Islamic jurisprudence—total erasure of a verse''s legal force, or supersession in specific contexts while other applications remain? If classical scholars used ''abrogate'' more narrowly than modern readers assume, the apparent conflict between 9:5 and peaceful verses may be fabricated.',
    'Linguistic and jurisprudential analysis of classical nasikh doctrine across Maliki, Hanafi, Shafi''i, and Hanbali schools. If all schools recognize context-specific application rather than total abrogate, the contextual reading has deeper classical roots than the abrogating-universal reading claims.',
    'If classical nasikh is narrower than modern ''abrogate'', the contextual reading is not a modern innovation but a recovery of classical doctrine, strengthening its legitimacy and corroboration. The abrogating-universal reading would be an overstatement or distortion of classical jurisprudence. This would shift confidence in the reading''s founding-problem corroboration from medium to high.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(abrogation_doctrine_ambiguity, empirical, 'Whether classical nasikh doctrine supports context-specific application (favoring contextual-defensive) or total abrogate (supporting abrogating-universal).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_9_5_scope__contextual_defensive, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_9_5_scope__contextual_defensive, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(qura_tr_t0, observed).
narrative_ontology:measurement(qura_tr_t5, quran_9_5_scope__contextual_defensive, theater_ratio, 5, 0.1).
narrative_ontology:measurement_basis(qura_tr_t5, observed).
narrative_ontology:measurement(qura_tr_t10, quran_9_5_scope__contextual_defensive, theater_ratio, 10, 0.08).
narrative_ontology:measurement_basis(qura_tr_t10, observed).
narrative_ontology:measurement(qura_tr_t20, quran_9_5_scope__contextual_defensive, theater_ratio, 20, 0.07).
narrative_ontology:measurement_basis(qura_tr_t20, observed).
narrative_ontology:measurement(qura_tr_t30, quran_9_5_scope__contextual_defensive, theater_ratio, 30, 0.08).
narrative_ontology:measurement_basis(qura_tr_t30, projected).
narrative_ontology:measurement(qura_tr_t40, quran_9_5_scope__contextual_defensive, theater_ratio, 40, 0.09).
narrative_ontology:measurement_basis(qura_tr_t40, projected).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_9_5_scope__contextual_defensive, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(qura_be_t0, observed).
narrative_ontology:measurement(qura_be_t5, quran_9_5_scope__contextual_defensive, base_extractiveness, 5, 0.17).
narrative_ontology:measurement_basis(qura_be_t5, observed).
narrative_ontology:measurement(qura_be_t10, quran_9_5_scope__contextual_defensive, base_extractiveness, 10, 0.16).
narrative_ontology:measurement_basis(qura_be_t10, observed).
narrative_ontology:measurement(qura_be_t20, quran_9_5_scope__contextual_defensive, base_extractiveness, 20, 0.15).
narrative_ontology:measurement_basis(qura_be_t20, observed).
narrative_ontology:measurement(qura_be_t30, quran_9_5_scope__contextual_defensive, base_extractiveness, 30, 0.14).
narrative_ontology:measurement_basis(qura_be_t30, projected).
narrative_ontology:measurement(qura_be_t40, quran_9_5_scope__contextual_defensive, base_extractiveness, 40, 0.15).
narrative_ontology:measurement_basis(qura_be_t40, projected).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_9_5_scope__contextual_defensive, suppression_requirement, 0, 0.28).
narrative_ontology:measurement_basis(qura_su_t0, observed).
narrative_ontology:measurement(qura_su_t5, quran_9_5_scope__contextual_defensive, suppression_requirement, 5, 0.25).
narrative_ontology:measurement_basis(qura_su_t5, observed).
narrative_ontology:measurement(qura_su_t10, quran_9_5_scope__contextual_defensive, suppression_requirement, 10, 0.22).
narrative_ontology:measurement_basis(qura_su_t10, observed).
narrative_ontology:measurement(qura_su_t20, quran_9_5_scope__contextual_defensive, suppression_requirement, 20, 0.21).
narrative_ontology:measurement_basis(qura_su_t20, observed).
narrative_ontology:measurement(qura_su_t30, quran_9_5_scope__contextual_defensive, suppression_requirement, 30, 0.2).
narrative_ontology:measurement_basis(qura_su_t30, projected).
narrative_ontology:measurement(qura_su_t40, quran_9_5_scope__contextual_defensive, suppression_requirement, 40, 0.22).
narrative_ontology:measurement_basis(qura_su_t40, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_9_5_scope__contextual_defensive, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(quran_9_5_scope__contextual_defensive, 0.12).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__abrogating_universal).
narrative_ontology:affects_constraint(quran_9_5_scope__contextual_defensive, quran_9_5_scope__progressive_synthesis).

% DUAL FORMULATION NOTE:
% The Qur'an 9:5 constraint family consists of three structurally distinct readings instantiating different constraints from the same verse: (1) contextual_defensive (this constraint)—context-specific, defensive-only, enables pluralism; (2) abrogating_universal—universal abrogate of peaceful verses, standing offensive-jihad obligation, extracts from non-Muslims; (3) progressive_synthesis—time-bound directive, ethical trajectory supersedes literalism. The three readings compete for institutional adoption, scholarly authority, and political implementation. Each has its own ε, beneficiary/victim structure, and terminal classification. They are NOT alternate measurements of one constraint; they are genuinely different constraints that happen to debate the same verse. The constraint family is linked via network.affects_constraints to track institutional competition and corroboration flows.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
