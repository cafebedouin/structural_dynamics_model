% ============================================================================
% CONSTRAINT STORY: border_normative_status__freedom_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_border_normative_status__freedom_primary, []).

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
 *   constraint_id: border_normative_status__freedom_primary
 *   human_readable: Border Exclusion as Rights Violation (Freedom-Primary Reading)
 *   domain: political_philosophy/international_law/migration_studies
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'border normative status': the freedom-primary reading. Under this
 *   reading, freedom of movement is a fundamental human right that
 *   territorial borders impermissibly restrict; exclusion requires
 *   extraordinary justification that states cannot provide. The constraint is
 *   the institutional apparatus that denies this right through systematic
 *   border enforcement. From this reading's perspective, states benefit from
 *   suppressing the claim to free movement and construct justifications
 *   (sovereignty, security, fiscal sustainability) to maintain exclusion. The
 *   complementary readings (sovereignty_primary, qualified_sovereignty)
 *   contest this framing at the foundational level and declare different
 *   beneficiaries and victims. This story models the freedom-primary reading
 *   as a SNARE — pure extraction justified by coordination narratives that
 *   the reading rejects as false. The claim/metric divergence is intentional:
 *   this reading CLAIMS the constraint is a rights violation (snare), and the
 *   authored metrics (high extractiveness, high suppression, low
 *   coordination) describe extractive operation. A holder of the
 *   sovereignty_primary reading would classify the same institutional
 *   arrangement as legitimate coordination (rope or mountain) — per-seat
 *   classification via the engine will show this divergence.
 *
 * KEY AGENTS:
 *   - Excluded migrants: powerless, trapped, bear immediate costs of the constraint through barred movement and lost opportunity
 *   - Displaced domestic workers: powerless to constrained, bear biographical costs through inability to exit violence or collapse in home state
 *   - State enforcement apparatus: institutional power, administers the constraint, from their seat the enforcement is justified and legitimate
 *   - Receiving states: institutional power, claim benefit from exclusion but freedom-primary reading rejects the legitimacy of those claimed benefits
 *   - Asylum seekers: moderate power, identity-locked to the pursuit of refuge, occupy unstable position between nominal protection and systematic denial
 *   - Irregular migrants: powerless, trapped, face severest enforcement and complete illegality
 *   - International human rights bodies: analytical seat, document violation and contest state framing from outside beneficiary set
 *   - Receiving state populations: organized power, organized to receive claimed protections from border closure (labor market, welfare access), benefits are often illusory
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(border_normative_status__freedom_primary, 0.78).
domain_priors:suppression_score(border_normative_status__freedom_primary, 0.81).
domain_priors:theater_ratio(border_normative_status__freedom_primary, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, suppression_requirement, 0.81).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(border_normative_status__freedom_primary, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(border_normative_status__freedom_primary, snare).
narrative_ontology:human_readable(border_normative_status__freedom_primary, "Border Exclusion as Rights Violation (Freedom-Primary Reading)").
narrative_ontology:topic_domain(border_normative_status__freedom_primary, "political_philosophy/international_law/migration_studies").

domain_priors:requires_active_enforcement(border_normative_status__freedom_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(border_normative_status__freedom_primary, '5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f').
narrative_ontology:cs_kernel_codification('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', fixed_text).
narrative_ontology:cs_authority_grounding('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', lineage).
narrative_ontology:cs_interpretation_layer_present('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f').
narrative_ontology:cs_reading_relation('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', border_normative_status__qualified_sovereignty, influences).
narrative_ontology:cs_reading_relation('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', border_normative_status__sovereignty_primary, coexists_with).
narrative_ontology:cs_axiom('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', foundational, freedom_of_movement_fundamental_right).
narrative_ontology:cs_axiom_status(freedom_of_movement_fundamental_right, holdable).
narrative_ontology:cs_axiom_grounding('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', freedom_of_movement_fundamental_right, deontological).
narrative_ontology:cs_axiom('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', foundational, border_exclusion_requires_extraordinary_justification).
narrative_ontology:cs_axiom_status(border_exclusion_requires_extraordinary_justification, holdable).
narrative_ontology:cs_axiom_grounding('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', border_exclusion_requires_extraordinary_justification, deontological).
narrative_ontology:cs_reference_frame('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', universal_human_right_to_free_movement).
narrative_ontology:cs_drift_state('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', contemporary_militarized_borders, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('5ac64c7b-d8d5-43cb-8e26-08eff4be4c4f', '').
narrative_ontology:cs_kernel_id(border_normative_status__freedom_primary, border_normative_status).

% --- Structural relationships ---
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, excluded_migrants).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, displaced_domestic_workers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, asylum_seekers).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, irregular_migrants).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, receiving_states).
narrative_ontology:constraint_beneficiary(border_normative_status__freedom_primary, receiving_state_populations).
narrative_ontology:constraint_victim(border_normative_status__freedom_primary, border_enforcement_workers).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, universal_human_right_freedom_of_movement).
narrative_ontology:constraint_vindicates(border_normative_status__freedom_primary, human_dignity_primacy_over_state_claims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Barred from crossing borders by enforcement machinery (physical barriers, legal interdiction, visa denial, deportation). They bear the full cost of exclusion: inability to escape violence, access employment, reunite with family, or pursue opportunity. Exit from this constraint means border crossing itself, which the constraint forbids. Their structural position is complete enclosure — no alternatives exist within the constraint's operation.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded_migrants, payer,
    powerless, immediate, trapped, global).

% Citizens trapped in a home country that has become dangerous, economically failed, or ecologically collapsed. Border closure denies them exit. Unlike excluded migrants they retain nominal state membership (visa access, travel documents), but borders remain functionally closed by enforcement policy or by adjacent states' exclusion. They pay through continued exposure to the harms they cannot escape.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, displaced_domestic_workers, payer,
    powerless, biographical, constrained, national).

% Administers border enforcement: visa systems, physical barriers, interdiction patrols, deportation machinery. Maintains the legal and institutional apparatus that gives exclusion force. From their structural position the enforcement is justified as defending sovereignty and state interests; from the freedom-primary reading this same apparatus is a systematic human rights violation requiring justification it cannot provide.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, state_enforcement_apparatus, agenda_setter,
    institutional, generational, analytical, national).

% Enforce exclusion at their borders. They claim benefits: labor market protection, fiscal sustainability, cultural coherence, security. The freedom-primary reading identifies these claimed benefits as illegitimate — they are constructed to justify extraction from those excluded. The arrangement persists because states have enforcement capacity; removal would require fundamentally altering how borders operate.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, receiving_states, agenda_setter,
    institutional, generational, analytical, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, receiving_states, beneficiary).

% Flee persecution or life-threatening conditions and seek safe haven. Nominally protected by international refugee law, but enforcement is discretionary and refoulement (return to danger) remains systematic. They occupy an unstable middle ground: some recognition of their claim to entry, but institutional practice systematizes denial and pushback. Their identity as refugees binds them to the pursuit of asylum — they cannot simply abandon the need for safety.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, asylum_seekers, payer,
    moderate, biographical, identity_locked, global).

% Enter or remain without formal authorization. Face the severest enforcement: criminal penalties, detention, forced deportation, exclusion from legal protection. They are the primary targets of the enforcement machinery. Their structural position is complete illegality — recognized by neither the sending state (they fled) nor the receiving state (they crossed without authorization). Exit means either voluntary deportation or perpetual illegality.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, irregular_migrants, payer,
    powerless, immediate, trapped, global).

% Conduct the work of exclusion: patrol borders, process asylum claims, execute deportations, staff detention. From the freedom-primary reading they are complicit in systematic rights violation. Many hold the position because it is employment; exiting requires alternative livelihood. Some identify with enforcement mission; for them exit is ideologically costly. They are both instruments of the constraint and trapped within it.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, border_enforcement_workers, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(border_normative_status__freedom_primary, border_enforcement_workers, agenda_setter).

% Monitor and document border violations: UN bodies, regional human rights courts, NGO networks. Document the extraction and suppression. They possess no enforcement power over states but generate the testimony and analysis that contests the state framing. Their analytical seat is the primary voice available to challenge the constraint's legitimacy from outside the benefiting parties.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, international_human_rights_bodies, observer,
    institutional, generational, analytical, global).

% Receive claimed economic and social protections from border closure: labor market segmentation (domestic workers nominally protected from competition), welfare access restricted to citizens, cultural membership defined by exclusion. They benefit, but benefits are diffuse and often illusory (economic gains from exclusion are modest compared to documented harms of closed borders). Exit from this role means acknowledging that freedom of movement benefits receiving populations too.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, receiving_state_populations, beneficiary,
    organized, biographical, constrained, national).

% Those whose claims to movement would be strongest under the freedom-primary reading but who are absent from the conversation: children in transit, stateless persons, climate refugees, trafficked persons. They cannot represent their interests because their exclusion is total and the constraint structures prevent their voice from entering the discussion.
narrative_ontology:constraint_stakeholder(border_normative_status__freedom_primary, excluded, excluded,
    powerless, immediate, trapped, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(border_normative_status__freedom_primary, receiving_states).
narrative_ontology:fixing_cost_class(border_normative_status__freedom_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: None. The constraint is presented as coordination (protecting state capacity, managing migration flows) but the freedom-primary reading rejects this framing. The actual function is unidirectional extraction.
% TRANSFER_FUNCTION: Moves the freedom to move FROM excluded populations TO receiving state authorities and citizens. The constraint transfers the ability to control movement, labor access, and social belonging from migrants to state enforcement systems and receiving populations. It transfers risk: migrants bear vulnerability; states and citizens claim security.
% ABSENT_VOICES: Those most completely excluded: stateless persons with no state to claim them, children in parental custody with no independent voice, climate refugees whose displacement preceded formal recognition, persons in the process of crossing (at-sea, in transit camps) with no territorial anchor. The freedom-primary reading would amplify their claims, but the institutional structure prevents their presence in the conversation. Also absent: migrant workers' own organizations in origin countries that contest the framing that movement is a privilege rather than a right.
% DISAPPEARANCE_RATIONALE: If this constraint (border exclusion as institutionalized) disappeared, global migration patterns would shift dramatically within months: labor flows would redistribute, diaspora communities would reunify, refuge pathways would open. State fiscal models and labor markets currently calibrated to closed borders would reorganize. The political identity of nation-states built on bounded territorial membership would destabilize. The receiving state populations' claimed protections would evaporate or require defensive reorganization. The world deeply depends on this constraint's operation.
% FOUNDING_PROBLEM: The founding problem, from the freedom-primary perspective, is NOT the classic sovereignty problem (how do states coordinate territory). Rather, it is the problem of justifying exclusion in a world with global inequality, violence, and resource scarcity — a problem that CANNOT be solved by border enforcement alone. The constraint was constructed not to solve coordination but to SUPPRESS the question of whether borders are legitimate at all.
% FOUNDING_PROBLEM_CORROBORATION: International human rights bodies and migration scholars argue the founding problem is a constructed justification for extraction, not a genuine coordination problem: borders do not inherently require exclusion (open-border thought experiments and historical periods of free movement demonstrate alternatives). States and security establishments counter that orderly migration management requires borders. This is genuinely contested; no external corroboration resolves it because the disagreement is at the level of normative framing (what IS the founding problem?), not empirical fact.
narrative_ontology:disappearance_verdict(border_normative_status__freedom_primary, world_rearranges).
narrative_ontology:founding_problem_status(border_normative_status__freedom_primary, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(border_normative_status__freedom_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(border_normative_status__freedom_primary, 'none', 1).
narrative_ontology:epsilon_provenance(border_normative_status__freedom_primary, 0.78, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(border_normative_status__freedom_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(border_normative_status__freedom_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(border_normative_status__freedom_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.78 at interval end) is high because the constraint unilaterally denies a claimed fundamental right; there is no compensation mechanism or equity principle — the extraction is pure. Suppression (0.81) is higher still because the constraint's persistence depends entirely on active enforcement (border patrols, visa denial, deportation, detention); remove enforcement machinery and the constraint collapses within days. Theater (0.42) is moderate because states do conduct genuine security review and some migration management is administratively necessary, but an increasing share of enforcement machinery (walls, interdiction at sea, detention complexes) serves pure exclusion, not coordination. Accessibility collapse (0.72) is high because alternatives to the constraint are actively foreclosed: irregular routes face militarization; legal pathways are narrowed; statelessness persists by design. Resistance (0.68) is substantial because excluded populations, diaspora organizations, refugee movements, and human rights bodies mount continuous contestation — the constraint persists through suppression, not consent. The measurement series tracks extraction accumulation from 1945 (post-UDHR founding, when freedom of movement was nominally universal) through 2024 (contemporary intensified enforcement): extractiveness and suppression have both risen ~1.8x, indicating the original coordination justification (post-WWII: preventing statelessness, managing displaced persons) has been wholly displaced by extraction logic. Theater ratio has risen more slowly (1.9x), suggesting performative justification has intensified but maintains below-threshold proportions. The coercion grid differentiates levels: individual-level accessibility collapse has risen most sharply (0.58→0.78) because digital surveillance and biometric borders now make escape attempts detectable at the individual level; organizational resistance has held steady or grown slightly because migrant worker organizations, diaspora networks, and refugee advocacy bodies have strengthened; class-level collapse shows the highest persistent pressure because entire classes (low-skill workers, climate-displaced, those fleeing state violence) face uniform exclusion regardless of individual merit; structural-level dynamics show more moderate rise because alternatives (open-borders thought experiments, regional free-movement zones like Schengen pre-closure) remain intellectually available even as institutionally foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seats (state enforcement apparatus, receiving states) and the victim seats (excluded migrants, displaced workers, asylum seekers) should compute radically different constraint types from the same structural data. From the enforcement apparatus position, borders are legitimate coordination mechanisms (rope or mountain — natural boundaries of political membership). From the victim seats, the same institutional apparatus is systematized rights violation (snare). The freedom-primary reading computes this divergence as correct and structural, not as disagreement about facts. The directionality difference is stark: enforcement apparatus approaches d=0.0 (beneficiary of the coordination it administers), while excluded migrants approach d=1.0 (full targets). The engine's per-seat computation will show this radical divergence — it is not a failure of the story but the intended measurement of conflicting normative frames grounded in different structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Excluded migrants: d approaches 1.0 (full target). They bear the constraint entirely; no benefit attaches to them; their exit from the constraint is the constraint itself. Power is powerless, time horizon immediate (they face life-or-death decisions), exit is trapped (no legal route, illegal routes face lethal enforcement). Displaced domestic workers: d approaches 0.95. They are targets of a derivative exclusion (unable to flee even their own state); minimal benefit, comprehensive cost. Asylum seekers: d approaches 0.85. Nominally protected by international law but systematically denied; identity-locked to the pursuit of refuge (cannot simply abandon the need for safety), so exit is theoretically available but ideologically foreclosed. Irregular migrants: d approaches 1.0 (full target). They face the severest suppression; no legal pathway; complete illegality. Border enforcement workers: d approaches 0.65. They are instruments of extraction but also partially trapped (employment, ideological commitment); secondary_role payer recognizes they pay a cost of complicity even as they agenda-set. Receiving states: d approaches 0.15 (beneficiary). They collect the entire benefit of exclusion (labor market segmentation, welfare restrictions, cultural boundary maintenance); they administer the constraint; their power is institutional. Receiving state populations: d approaches 0.25 (weak beneficiary). They receive claimed labor market protections and welfare access restriction, but benefits are diffuse and often economically illusory (closed borders reduce overall growth); they do not administer the constraint. International human rights bodies: d approaches 0.5 (analytical/symmetric). They neither benefit nor pay from the constraint's operation; their position is to contest it from outside. These directionality assignments are derived from beneficiary/victim declarations + power + exit options; no overrides are necessary because the structural data itself produces the divergence.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's founding problem (post-WWII international order: preventing statelessness, managing displaced populations after genocide and colonialism) was live in 1945. By 1970 it was substantially dead: statelessness persisted, displaced person camps became permanent, and the constraint's justification had shifted from humanitarian coordination to nationalist extraction. By 2024 the founding problem is deceased but the constraint persists at higher intensity (0.78 extractiveness vs. initial 0.42). This meets the mandatrophy condition: a once-justified arrangement whose justification has evaporated but whose enforcement has intensified. The engine's mismatch detector (founding_problem_status=dead + disappearance_verdict=world_rearranges) will flag this as a zombie constraint — it persists because beneficiaries (receiving states) have institutional power to maintain it, not because the founding coordination problem remains live. The freedom-primary reading declares mandatrophy explicitly: border enforcement became pure extraction once the founding problem was solved. The alternative readings (sovereignty_primary, qualified_sovereignty) will contest this diagnosis by reframing the founding problem: not 'prevent statelessness' but 'maintain territorial collective self-determination,' which they would declare live. The mandatrophy diagnosis is reading-dependent — it rests on the freedom-primary claim that movement is a fundamental right, therefore restriction requires ever-stronger justification that states cannot provide.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    rights_naturality_vs_construction,
    'Is freedom of movement a natural right discoverable in human nature, or is it a constructed normative claim grounded in specific historical conditions (post-WWII humanitarian discourse, liberalism)?',
    'Genealogical analysis of the right''s emergence (did it originate as universal claim or as post-colonial response to statelessness?); cross-cultural philosophy examining whether the right appears in non-Western normative traditions or is a Western transplant.',
    'If natural/universal, the freedom-primary reading''s claim that borders violate a fundamental right stands. If constructed, the reading becomes one normative frame among several with no special ontological standing; the qualified_sovereignty and sovereignty_primary readings gain legitimacy as alternative constructions.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rights_naturality_vs_construction, conceptual, 'Whether freedom of movement is a discoverable natural right or a constructed post-WWII normative claim.').

omega_variable(
    state_exit_vs_individual_exit,
    'Does freedom of movement have a meaningful content if states retain right of refusal (qualified_sovereignty frame), or does border authority necessarily negate the right?',
    'Thought experiment: can a world exist where states retain exclusion authority but exercise it rarely and for demonstrably legitimate reasons (defense, disease prevention) without undermining the right''s claim to fundamentality? If yes, freedom-primary and qualified_sovereignty may be reconcilable. If no, the readings genuinely foreclose each other.',
    'If reconcilable, the qualified_sovereignty reading emerges as a compromise position. If not, the freedom-primary reading''s core claim (borders as rights violation) is fundamentally opposed to any version of state border authority, making the foreclosure relation stronger.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_exit_vs_individual_exit, conceptual, 'Whether state border authority can coexist with freedom of movement as fundamental right.').

omega_variable(
    extraction_vs_legitimate_preference,
    'Is the claimed benefit receiving state populations experience from border closure (labor market protection, welfare access restriction, cultural boundary maintenance) extraction imposed on migrants, or a legitimate preference of receiving communities?',
    'Empirical: does closing borders actually protect receiving-state workers'' wages and employment (evidence is mixed — most studies show minimal labor market effects from immigration restrictions)? Normative: even if benefits exist, are they legitimate grounds to restrict fundamental rights claimed by others? Who decides legitimacy?',
    'If benefits are illusory, the constraint is pure extraction with no coordination component. If benefits are real and legitimately weighted, the constraint may be a tangled_rope (genuine coordination with extraction overlay) rather than a pure snare. If legitimacy is contested, the qualified_sovereignty frame emerges as the institutional compromise that weights both claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_vs_legitimate_preference, empirical, 'Whether claimed labor market and welfare benefits of border closure are real and legitimately weighed.').

omega_variable(
    stateless_persons_identity_lock,
    'For stateless persons with no territorial anchor (no sending state, no receiving state), what is their relationship to the border constraint? Are they trapped by identity-lock (lack of documents, no state to advocate for them) or by pure structural powerlessness?',
    'Comparative analysis of stateless persons'' exit options: do they have any legal pathway, any negotiating power with any state, any arbitrage route? Or is the constraint literally absolute — no alternative exists structurally?',
    'If trapped by identity-lock (documentlessness creates a binding but theoretically-breakable condition), the suppression metric underestimates because documents are a created barrier, not a natural fact. If trapped absolutely, the extractiveness metric understates because there is no exit at any theoretical cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stateless_persons_identity_lock, empirical, 'Whether stateless persons face identity-locked or absolute-structural trapping under border exclusion.').

omega_variable(
    sovereign_reading_internal_contradiction,
    'Does the sovereignty_primary reading (states have foundational authority to exclude) contain an internal contradiction when states derive legitimacy from the consent of the governed, which excluded migrants cannot give?',
    'Formal analysis: can a state claim legitimacy via popular sovereignty while excluding populations that states'' own policing creates (people fleeing state violence are displaced by state action, then excluded by state authority for fleeing that action)?',
    'If contradiction is fatal, sovereignty_primary forecloses itself rather than just being contested by freedom-primary. If contradiction is resolvable (e.g., via appeal to pre-existing membership or cultural boundaries), the reading remains defensible and coexists with freedom-primary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereign_reading_internal_contradiction, conceptual, 'Whether sovereignty_primary reading''s grounding in popular consent internally contradicts its exclusion of displaced persons.').

omega_variable(
    kernel_reading_vs_pure_empirical_constraint,
    'Is the border normative status kernel genuinely alive as a contestable framework, or have receiving-state institutions already foreclosed the freedom-primary reading through institutional embedding and normalized enforcement?',
    'Genealogy: is freedom-primary reading still advanced as a serious legal/political position by major actors (governments, international bodies, transnational movements), or has it been relegated to NGO / academic margins? Does the kernel remain open to political reversal, or is the receiving-state reading so institutionalized that the others are museum pieces?',
    'If the kernel remains contestable, all three readings coexist and the committer frame is appropriate. If one reading has been institutionally foreclosed, the kernel is degraded and the constraint becomes a standard (non-kernel) story of institutions maintaining extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_vs_pure_empirical_constraint, empirical, 'Whether the border normative status kernel remains genuinely contestable or has been foreclosed by institutional embedding.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(border_normative_status__freedom_primary, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bord_tr_t1945, border_normative_status__freedom_primary, theater_ratio, 1945, 0.22).
narrative_ontology:measurement_basis(bord_tr_t1945, projected).
narrative_ontology:measurement(bord_tr_t1970, border_normative_status__freedom_primary, theater_ratio, 1970, 0.28).
narrative_ontology:measurement_basis(bord_tr_t1970, observed).
narrative_ontology:measurement(bord_tr_t1990, border_normative_status__freedom_primary, theater_ratio, 1990, 0.33).
narrative_ontology:measurement_basis(bord_tr_t1990, observed).
narrative_ontology:measurement(bord_tr_t2005, border_normative_status__freedom_primary, theater_ratio, 2005, 0.37).
narrative_ontology:measurement_basis(bord_tr_t2005, observed).
narrative_ontology:measurement(bord_tr_t2015, border_normative_status__freedom_primary, theater_ratio, 2015, 0.4).
narrative_ontology:measurement_basis(bord_tr_t2015, observed).
narrative_ontology:measurement(bord_tr_t2024, border_normative_status__freedom_primary, theater_ratio, 2024, 0.42).
narrative_ontology:measurement_basis(bord_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(bord_be_t1945, border_normative_status__freedom_primary, base_extractiveness, 1945, 0.42).
narrative_ontology:measurement_basis(bord_be_t1945, projected).
narrative_ontology:measurement(bord_be_t1970, border_normative_status__freedom_primary, base_extractiveness, 1970, 0.54).
narrative_ontology:measurement_basis(bord_be_t1970, observed).
narrative_ontology:measurement(bord_be_t1990, border_normative_status__freedom_primary, base_extractiveness, 1990, 0.62).
narrative_ontology:measurement_basis(bord_be_t1990, observed).
narrative_ontology:measurement(bord_be_t2005, border_normative_status__freedom_primary, base_extractiveness, 2005, 0.71).
narrative_ontology:measurement_basis(bord_be_t2005, observed).
narrative_ontology:measurement(bord_be_t2015, border_normative_status__freedom_primary, base_extractiveness, 2015, 0.75).
narrative_ontology:measurement_basis(bord_be_t2015, observed).
narrative_ontology:measurement(bord_be_t2024, border_normative_status__freedom_primary, base_extractiveness, 2024, 0.78).
narrative_ontology:measurement_basis(bord_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(bord_su_t1945, border_normative_status__freedom_primary, suppression_requirement, 1945, 0.48).
narrative_ontology:measurement_basis(bord_su_t1945, projected).
narrative_ontology:measurement(bord_su_t1970, border_normative_status__freedom_primary, suppression_requirement, 1970, 0.58).
narrative_ontology:measurement_basis(bord_su_t1970, observed).
narrative_ontology:measurement(bord_su_t1990, border_normative_status__freedom_primary, suppression_requirement, 1990, 0.67).
narrative_ontology:measurement_basis(bord_su_t1990, observed).
narrative_ontology:measurement(bord_su_t2005, border_normative_status__freedom_primary, suppression_requirement, 2005, 0.74).
narrative_ontology:measurement_basis(bord_su_t2005, observed).
narrative_ontology:measurement(bord_su_t2015, border_normative_status__freedom_primary, suppression_requirement, 2015, 0.78).
narrative_ontology:measurement_basis(bord_su_t2015, observed).
narrative_ontology:measurement(bord_su_t2024, border_normative_status__freedom_primary, suppression_requirement, 2024, 0.81).
narrative_ontology:measurement_basis(bord_su_t2024, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1945, tn=2024
narrative_ontology:measurement(bord_grid_01, border_normative_status__freedom_primary, accessibility_collapse(class), 1945, 0.62).
narrative_ontology:measurement(bord_grid_02, border_normative_status__freedom_primary, accessibility_collapse(class), 2024, 0.82).
narrative_ontology:measurement(bord_grid_03, border_normative_status__freedom_primary, accessibility_collapse(individual), 1945, 0.58).
narrative_ontology:measurement(bord_grid_04, border_normative_status__freedom_primary, accessibility_collapse(individual), 2024, 0.78).
narrative_ontology:measurement(bord_grid_05, border_normative_status__freedom_primary, accessibility_collapse(organizational), 1945, 0.48).
narrative_ontology:measurement(bord_grid_06, border_normative_status__freedom_primary, accessibility_collapse(organizational), 2024, 0.71).
narrative_ontology:measurement(bord_grid_07, border_normative_status__freedom_primary, accessibility_collapse(structural), 1945, 0.55).
narrative_ontology:measurement(bord_grid_08, border_normative_status__freedom_primary, accessibility_collapse(structural), 2024, 0.75).
narrative_ontology:measurement(bord_grid_09, border_normative_status__freedom_primary, resistance(class), 1945, 0.41).
narrative_ontology:measurement(bord_grid_10, border_normative_status__freedom_primary, resistance(class), 2024, 0.71).
narrative_ontology:measurement(bord_grid_11, border_normative_status__freedom_primary, resistance(individual), 1945, 0.32).
narrative_ontology:measurement(bord_grid_12, border_normative_status__freedom_primary, resistance(individual), 2024, 0.68).
narrative_ontology:measurement(bord_grid_13, border_normative_status__freedom_primary, resistance(organizational), 1945, 0.28).
narrative_ontology:measurement(bord_grid_14, border_normative_status__freedom_primary, resistance(organizational), 2024, 0.72).
narrative_ontology:measurement(bord_grid_15, border_normative_status__freedom_primary, resistance(structural), 1945, 0.38).
narrative_ontology:measurement(bord_grid_16, border_normative_status__freedom_primary, resistance(structural), 2024, 0.62).
narrative_ontology:measurement(bord_grid_17, border_normative_status__freedom_primary, stakes_inflation(class), 1945, 0.48).
narrative_ontology:measurement(bord_grid_18, border_normative_status__freedom_primary, stakes_inflation(class), 2024, 0.81).
narrative_ontology:measurement(bord_grid_19, border_normative_status__freedom_primary, stakes_inflation(individual), 1945, 0.42).
narrative_ontology:measurement(bord_grid_20, border_normative_status__freedom_primary, stakes_inflation(individual), 2024, 0.74).
narrative_ontology:measurement(bord_grid_21, border_normative_status__freedom_primary, stakes_inflation(organizational), 1945, 0.35).
narrative_ontology:measurement(bord_grid_22, border_normative_status__freedom_primary, stakes_inflation(organizational), 2024, 0.68).
narrative_ontology:measurement(bord_grid_23, border_normative_status__freedom_primary, stakes_inflation(structural), 1945, 0.44).
narrative_ontology:measurement(bord_grid_24, border_normative_status__freedom_primary, stakes_inflation(structural), 2024, 0.76).
narrative_ontology:measurement(bord_grid_25, border_normative_status__freedom_primary, suppression(class), 1945, 0.52).
narrative_ontology:measurement(bord_grid_26, border_normative_status__freedom_primary, suppression(class), 2024, 0.86).
narrative_ontology:measurement(bord_grid_27, border_normative_status__freedom_primary, suppression(individual), 1945, 0.44).
narrative_ontology:measurement(bord_grid_28, border_normative_status__freedom_primary, suppression(individual), 2024, 0.84).
narrative_ontology:measurement(bord_grid_29, border_normative_status__freedom_primary, suppression(organizational), 1945, 0.38).
narrative_ontology:measurement(bord_grid_30, border_normative_status__freedom_primary, suppression(organizational), 2024, 0.77).
narrative_ontology:measurement(bord_grid_31, border_normative_status__freedom_primary, suppression(structural), 1945, 0.48).
narrative_ontology:measurement(bord_grid_32, border_normative_status__freedom_primary, suppression(structural), 2024, 0.81).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(border_normative_status__freedom_primary, global_infrastructure).
narrative_ontology:boltzmann_floor_override(border_normative_status__freedom_primary, 0.25).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__qualified_sovereignty).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, border_normative_status__sovereignty_primary).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, labor_market_segmentation_migrant_workers).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, international_refugee_convention_enforcement).
narrative_ontology:affects_constraint(border_normative_status__freedom_primary, statelessness_persistence).

% DUAL FORMULATION NOTE:
% This constraint is part of a constraint family decomposed from the single natural-language concept 'border normative status.' Three structurally distinct readings instantiate different constraints with different victims, beneficiaries, and ε values: freedom_primary (this story, high extraction, freedom of movement as fundamental right); qualified_sovereignty (legitimate-but-constrained authority, hybrid coordination/extraction); sovereignty_primary (states have foundational authority, presents as coordination or mountain). The family is linked via network.affects_constraints: freedom_primary and sovereignty_primary directly contest the foundational framing, while qualified_sovereignty attempts institutional compromise. Each reading has distinct axioms, distinct victim sets (excluded migrants; displaced workers; state sovereignty claimants), and distinct foundational premises. The decomposition applies the ε-invariance principle: measuring the constraint under freedom-primary axioms yields high ε and high suppression; measuring under sovereignty_primary axioms would yield low ε and low suppression. Rather than forcing one story with an observable parameter, three stories with stable ε values per reading create the necessary measurement infrastructure.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(border_normative_status__freedom_primary, organized, 0.35).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
