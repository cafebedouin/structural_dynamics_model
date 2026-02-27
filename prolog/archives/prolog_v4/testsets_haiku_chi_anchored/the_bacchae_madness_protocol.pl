% ============================================================================
% CONSTRAINT STORY: the_bacchae_madness_protocol
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_the_bacchae_madness_protocol, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: the_bacchae_madness_protocol
 *   human_readable: The Dionysian Mandate of Ecstasy
 *   domain: religious/political/social
 *
 * SUMMARY:
 *   The Dionysian Mandate of Ecstasy models a structural collision between
 *   two incommensurable orders: the rational, rule-based civic authority of
 *   King Pentheus and the chaotic, ecstatic mandate of the god Dionysus. The
 *   constraint is not the god himself (who may or may not exist) but the
 *   social mechanism that uses divine authority to justify coercive inclusion
 *   into an ecstatic cult. Women are afflicted with madness if they refuse
 *   initiation; citizens who mock the god are destroyed (Pentheus); the
 *   priesthood maintains power by threatening supernatural punishment for
 *   resistance. The constraint exhibits the full spectrum of DR types
 *   depending on observational position: the priesthood experiences it as
 *   pure coordination (Rope), the unconverted victims experience it as pure
 *   extraction (Snare), the initiated women experience it as mixed (Tangled
 *   Rope), the rational authority experiences it as a trap (Snare), and a
 *   long-term analyst sees it as performative theater (Piton). The
 *   constraint's extractiveness has grown over time as the priesthood refines
 *   its mechanisms for orchestrating madness and public terror.
 *
 * KEY AGENTS:
 *   - King Pentheus: Powerful but trapped (powerful/trapped) — attempts to suppress the cult through law; encounters the mandate as a lethal snare
 *   - Dionysian Priesthood: Institutional beneficiary (institutional/arbitrage) — controls the cult apparatus; frames extraction as coordination
 *   - Unconverted Citizens / Non-Initiated Women: Powerless victims (powerless/trapped) — face coercive inclusion through madness; no exit except compliance
 *   - Bacchae / Initiated Women: Moderate mixed participants (moderate/mobile) — experience liberation from household constraints and also subordination to divine frenzy
 *   - The God Dionysus: Ambiguous agent (varies by perspective) — may be autonomous supernatural force or mythological cover for priestly control
 *   - Rational Civic Order: Abstract victim (powerful in principle, trapped in practice) — the institutional framework that cannot coexist with the chaotic mandate
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(the_bacchae_madness_protocol, 0.68).
domain_priors:suppression_score(the_bacchae_madness_protocol, 0.78).
domain_priors:theater_ratio(the_bacchae_madness_protocol, 0.65).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(the_bacchae_madness_protocol, extractiveness, 0.68).
narrative_ontology:constraint_metric(the_bacchae_madness_protocol, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(the_bacchae_madness_protocol, theater_ratio, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(the_bacchae_madness_protocol, snare).
narrative_ontology:human_readable(the_bacchae_madness_protocol, "The Dionysian Mandate of Ecstasy").
narrative_ontology:topic_domain(the_bacchae_madness_protocol, "religious/political/social").

domain_priors:requires_active_enforcement(the_bacchae_madness_protocol).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(the_bacchae_madness_protocol, dionysian_priesthood).
narrative_ontology:constraint_beneficiary(the_bacchae_madness_protocol, ecstatic_initiates).
narrative_ontology:constraint_victim(the_bacchae_madness_protocol, rational_civic_order).
narrative_ontology:constraint_victim(the_bacchae_madness_protocol, non_initiated_populace).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: UNCONVERTED CITIZEN (SNARE) — Citizens who do not participate in the Dionysian cult face coercion disguised as divine mandate. Madness is weaponized: women who resist initiation are afflicted with frenzy; those who mock the god face public destruction (Pentheus). No exit exists except compliance or death. d≈0.92, f(d)≈1.38, σ=0.8 → χ≈0.73.
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: KING PENTHEUS / RATIONAL AUTHORITY (SNARE) — Pentheus attempts to suppress the Dionysian cult through law and force. He believes himself trapped between losing authority (if he tolerates the cult) or being destroyed (if he resists). His resistance to the 'divine mandate' is reframed as sacrilege. Dionysus drives him to madness and death disguised as initiation. d≈0.88, f(d)≈1.33, σ=0.8 → χ≈0.70. High extraction despite (or because of) his power — the snare is designed to trap the most resistant.
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, snare,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: DIONYSIAN PRIESTHOOD (ROPE) — The priesthood frames participation as pure coordination: the god requires celebration, ecstasy enables collective bonding, ritual ensures community cohesion and divine favor. From this perspective, the 'extraction' is coordination benefit. Theater is performative but functional (ritual works). They experience no suppression — the mandate is liberation. d≈0.08, f(d)≈-0.11, σ=0.8 → χ≈-0.06. Net beneficiary.
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: INITIATED WOMEN / BACCHAE (TANGLED ROPE) — Initiated women experience both liberation (escape from household constraints, ecstatic sensation, communal power) and constraint (the mandate to serve Dionysus is absolute, disobedience causes madness or death, individual will is subordinated to divine frenzy). Exit exists nominally but at high cost — refusing the god induces affliction. d≈0.52, f(d)≈0.63, σ=0.8 → χ≈0.33. Mixed coordination (genuine community benefit) and extraction (subordination to divine will).
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: THE ANALYST / PERFORMATIVE VIEW (PITON) — From a long-term perspective, the Dionysian mandate is sustained largely through theatrical maintenance: the ritual spectacle, the myth of divine presence, and the terror of punishment. The underlying 'extraction' mechanism — actual divine power — is indefensible; instead, the priesthood maintains compliance through orchestrated madness, public examples (Pentheus's death), and performed ecstasy. theater_ratio=0.65 suggests substantial performative maintenance. The mandate persists through institutional inertia and mythological authority rather than through genuine coordination value. d≈0.70, f(d)≈1.12, σ=1.0 → χ≈0.73.
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a universal civilizational scale, the constraint appears as an immutable aspect of human consciousness itself: the capacity for ecstatic experience, collective frenzy, and surrender to overwhelming sensation is a natural human limit that societies must either channel (through ritual) or suppress (at psychological cost). Dionysus is the god of an inherent feature of human nature — the constraint is not extractive but emergent from human neural architecture and social bonding capacity. This perspective naturalizes the constraint. However, structural data (ε=0.68, suppression=0.78) contradicts the mountain classification — the engine flags this as a false summit. The 'natural human limit' framing masks the political use of ecstasy as a control mechanism.
constraint_indexing:constraint_classification(the_bacchae_madness_protocol, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(the_bacchae_madness_protocol_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(the_bacchae_madness_protocol, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(the_bacchae_madness_protocol, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(the_bacchae_madness_protocol, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(the_bacchae_madness_protocol, TR),
    TR >= 0.70.

:- end_tests(the_bacchae_madness_protocol_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The mandate begins as a regional religious practice (ε≈0.35) but grows into a coercive system extracting participation, labor, and absolute obedience. The extraction mechanism is sophistic: resistance to the 'divine will' is reframed as sacrilege, punishable by madness or death. By the end of the interval (point 7), the priesthood has refined its control — public examples (Pentheus), orchestrated afflictions, and the mythological monopoly on truth about the god's preferences create an extraction apparatus that rivals any political institution. Suppression (0.78): Very high. The mandate suppresses: rational discourse about the god's nature (questioning is sacrilege), individual choice about participation (refusal triggers affliction), and alternative narratives about the priesthood's interests (the narrative is always 'the god demands'). Terror is the primary suppression mechanism — the knowledge that resistance causes public destruction (Pentheus torn apart by his own mother) ensures compliance through fear, not persuasion. Theater ratio (0.65): Moderate-high. The ritual is performative — the spectacle of ecstasy, the choreography of madness, the mythological narrative of divine presence — but it has genuine psychological effects (group euphoria, altered consciousness from ritual drugs and hyperventilation) and real social consequences (coercive initiation, public punishment). The theater is functional in maintaining control even if the underlying divine agency claim is fraudulent.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates maximum perspectival divergence. The priesthood sees pure coordination (Rope) — the god requires celebration, the ritual works, no extraction. The unconverted victims see pure extraction (Snare) — coercive inclusion disguised as divine mandate, terror and madness as enforcement. Pentheus (rational authority) sees a lethal trap (Snare) — he cannot tolerate the cult without losing authority, cannot suppress it without triggering the god's (priesthood's) destruction. The initiated women see mixed coordination and extraction (Tangled Rope) — genuine liberation from household constraints alongside absolute subordination to divine will. The long-term analyst sees performative theater (Piton) — a degraded mandate maintained through mythological inertia and orchestrated terror rather than genuine divine power or coordination function. The civilizational observer risks seeing an immutable natural law (Mountain) — ecstatic experience as an inherent feature of human consciousness — but the structural data reveals this as a false summit: the mandate is a contingent political mechanism using ecstasy as a control tool.
 *
 * DIRECTIONALITY LOGIC:
 *   Dionysian Priesthood: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.11. Net beneficiary. They control narrative, accumulate followers, concentrate power. Unconverted Citizens: Victim + trapped → d≈0.92, f(d)≈1.38. Maximum extraction. No exit except compliance or death. King Pentheus: Victim + trapped despite power → d≈0.88, f(d)≈1.33. High extraction even for the powerful — the snare is designed to trap resistance. His power cannot overcome the mythological authority of the mandate. Bacchae / Initiated Women: Mixed (beneficiary of liberation + victim of subordination) + mobile → d≈0.52, f(d)≈0.63. The exit option (mobile) reflects their ability to participate in ecstatic ritual as a chosen identity, but the cost of exit (abandonment of community, loss of ritual benefits, social ostracism) makes it nominally mobile but practically constrained. Rational Civic Order: Victim + trapped (institutional scale) → d≈0.85, f(d)≈1.20. The entire rational authority structure is incompatible with the chaotic mandate and cannot coexist without subordination.
 *
 * MANDATROPHY ANALYSIS:
 *   MANDATROPHY PRESENT BUT UNRESOLVED. The constraint exhibits classic mandatrophy because it could plausibly be classified as either pure coordination (Rope: ecstatic ritual does create genuine community bonding) or pure extraction (Snare: the mandate coerces participation and extracts obedience through terror). The perspectives reveal the resolution: the constraint is coordinating AND extractive simultaneously (Tangled Rope from the moderate perspective), but the priesthood's false claim that it is pure coordination (Rope) and the victim's true experience that it is pure extraction (Snare) diverge. The mandatrophy is resolved by recognizing that the constraint's type depends on structural position: beneficiaries experience Rope, victims experience Snare, and the true type (visible at the analytical level) is Tangled Rope — real coordination function (group bonding, ritual efficacy) combined with asymmetric extraction (coercive inclusion, priesthood power concentration, subordination of individual will). The false mountain perspective (naturalizing ecstasy as inherent human limit) is the highest-risk mandatrophy reading — it legitimizes the constraint by mystifying it as natural law rather than political mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_agency_vs_priestly_orchestration,
    'Is the madness afflicting those who resist the cult an autonomous divine force, or a psychophysiological phenomenon orchestrated by the priesthood through drugs, suggestion, and social pressure?',
    'Ethnographic comparison with other ecstatic religions; neurochemical analysis of substances used in Dionysian ritual; correlation between priestly presence and madness onset',
    'If truly divine: constraint is Mountain (immutable supernatural law). If orchestrated: constraint is Snare (extraction mechanism using ''divine mandate'' as cover).',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_agency_vs_priestly_orchestration, conceptual, 'Whether madness is autonomous or orchestrated').

omega_variable(
    authentic_liberation_vs_coercive_inclusion,
    'Does initiation into the Bacchae cult genuinely liberate women from household constraints, or does it substitute one form of subordination (to family/husband) for another (to Dionysus/priesthood)?',
    'Biographical narratives of initiated women; comparison of household roles before and after initiation; analysis of who benefits from women''s participation (husband loses labor, priesthood gains ritual performers)',
    'If genuine liberation: initiates experience Tangled Rope (real coordination + real extraction). If substitution: initiates experience Snare (coercive inclusion disguised as choice).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authentic_liberation_vs_coercive_inclusion, empirical, 'Whether initiation liberates or substitutes subordination').

omega_variable(
    sustainability_of_mandate_through_terror,
    'Can the Dionysian mandate persist if punishment for resistance (madness, death) becomes openly known as orchestrated rather than divine? What is the breaking point of the myth?',
    'Historical case studies of revealed priestcraft (where cult mechanisms become known); analysis of Pentheus narrative as a power demonstration; tracking of mandate resilience post-revelation',
    'If myth is fragile: constraint should degrade to Piton (theater-dependent). If myth is resilient: constraint is Snare (extraction persists even when orchestration is known).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sustainability_of_mandate_through_terror, preference, 'Whether mandate depends on mythological belief or persists through known coercion').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(the_bacchae_madness_protocol, 0, 7).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bacchae_tr_t0, the_bacchae_madness_protocol, theater_ratio, 0, 0.4).
narrative_ontology:measurement(bacchae_tr_t3, the_bacchae_madness_protocol, theater_ratio, 3, 0.52).
narrative_ontology:measurement(bacchae_tr_t7, the_bacchae_madness_protocol, theater_ratio, 7, 0.65).

% Extraction over time
narrative_ontology:measurement(bacchae_be_t0, the_bacchae_madness_protocol, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(bacchae_be_t3, the_bacchae_madness_protocol, base_extractiveness, 3, 0.52).
narrative_ontology:measurement(bacchae_be_t7, the_bacchae_madness_protocol, base_extractiveness, 7, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(the_bacchae_madness_protocol, enforcement_mechanism).
narrative_ontology:affects_constraint(the_bacchae_madness_protocol, theban_patriarchal_authority).
narrative_ontology:affects_constraint(the_bacchae_madness_protocol, ecstatic_altered_consciousness_limit).

% DUAL FORMULATION NOTE:
% The Dionysian Mandate is downstream of the natural human capacity for ecstatic experience (ecstatic_altered_consciousness_limit, ε≈0.15, Mountain) but represents a distinct structural constraint where that capacity is weaponized as a political control mechanism by the priesthood. The upstream Mountain constraint (ecstasy as natural limit) has high accessibility_collapse; the downstream Snare constraint (the mandate as coercive extraction) has high suppression. The family models how a natural human feature is instrumentalized into political power.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(the_bacchae_madness_protocol, powerful, 0.88).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
