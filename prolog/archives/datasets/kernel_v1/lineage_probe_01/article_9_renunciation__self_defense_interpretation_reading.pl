% ============================================================================
% CONSTRAINT STORY: article_9_renunciation__self_defense_interpretation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_9_self_defense_interpretation, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: article_9_renunciation__self_defense_interpretation_reading
 *   human_readable: Article 9 Self-Defense Interpretation Reading
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   Article 9 of the Japanese Constitution states: 'Aspiring sincerely to an
 *   international peace based on justice and order, the Japanese people
 *   forever renounce war as a sovereign right of the nation and the threat or
 *   use of force as means of settling international disputes. In order to
 *   accomplish the aim of the preceding paragraph, land, sea, and air forces,
 *   as well as other war potential, shall never be maintained.' For
 *   seventy-five years, Japan has maintained the Self-Defense Forces while
 *   nominally adhering to Article 9. This constraint models ONE READING of
 *   how Article 9's renunciation applies: the self-defense interpretation
 *   reading. This reading holds that Article 9 renounces the right to
 *   initiate aggressive war and prohibits maintaining forces aimed at
 *   offensive operations — but cannot renounce the inherent right of
 *   self-defense that precedes and survives any text. Therefore, forces
 *   maintained at the minimum necessary level for territorial defense are not
 *   'war potential' in Article 9's meaning. They are defense potential. The
 *   interpretation distinguishes defensive from offensive doctrines and
 *   constrains force levels to regional denial rather than global power
 *   projection. This reading coexists with two sibling readings: the
 *   absolute_pacifism_reading (Article 9 means what it says, the SDF is
 *   unconstitutional, and practice has simply outvoted the text), and the
 *   reinterpretation_2014_reading (the 2014 cabinet decision approving
 *   collective self-defense crossed the line and amended the constitution via
 *   executive reading rather than Article 96). This story instantiates the
 *   self-defense interpretation reading as a structurally distinct constraint
 *   with its own ε, beneficiaries, victims, and perspectival structure.
 *
 * KEY AGENTS:
 *   - Self-Defense Forces / SDF Institutional Authority: Primary beneficiary (institutional/arbitrage) — the self-defense interpretation is the mechanism by which the SDF's existence is constitutionally legitimized. The SDF experiences the constraint as solving the legal problem of its status.
 *   - US-Japan Alliance / Security Architecture: Primary beneficiary (institutional/arbitrage) — the interpretation enables Japan to maintain meaningful defense capacity while preserving Article 9 as a constraint on militarism. Alliance coordination is enhanced.
 *   - Literalist Pacifism / Article 9 Plain Text Authority: Primary victim (powerless/trapped) — the plain text's meaning is suppressed by the administrative reading. Literalists cannot exit: reading Article 9 plainly is now characterized as naive, and advocating for actual disarmament lacks political viability.
 *   - Constitutional Legalists / Amendment Advocates: Secondary actor (moderate/constrained) — believe Article 9 should be formally amended via Article 96. Experience both coordination (the self-defense interpretation stabilizes the SDF) and extraction (the interpretation bypasses constitutionally required amendment process).
 *   - Pacifist Civil Society / Peace Movement: Secondary actor (moderate/constrained) — the interpretation does establish constraints on militarism (offensive war prohibited, collective defense limited) while narrowing their understanding of Article 9's renunciation. Constrained exit.
 *   - Japanese Judiciary / Constitutional Courts: Institutional actor (institutional/arbitrage) — maintains performative constitutional review while deferring to executive on security matters. Piton perspective.
 *   - Analytical Observer: Universal/civilizational perspective (analytical/analytical) — risks naturalizing the self-defense interpretation as disclosure of pre-textual law rather than doctrinal choice.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_9_renunciation__self_defense_interpretation_reading, 0.38).
domain_priors:suppression_score(article_9_renunciation__self_defense_interpretation_reading, 0.48).
domain_priors:theater_ratio(article_9_renunciation__self_defense_interpretation_reading, 0.62).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_9_renunciation__self_defense_interpretation_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(article_9_renunciation__self_defense_interpretation_reading, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(article_9_renunciation__self_defense_interpretation_reading, theater_ratio, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_9_renunciation__self_defense_interpretation_reading, tangled_rope).
narrative_ontology:human_readable(article_9_renunciation__self_defense_interpretation_reading, "Article 9 Self-Defense Interpretation Reading").
narrative_ontology:topic_domain(article_9_renunciation__self_defense_interpretation_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(article_9_renunciation__self_defense_interpretation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_9_renunciation__self_defense_interpretation_reading, '7af98eee-7fea-4823-acfe-ffb9f231727f').
narrative_ontology:cs_kernel_codification('7af98eee-7fea-4823-acfe-ffb9f231727f', fixed_text).
narrative_ontology:cs_authority_grounding('7af98eee-7fea-4823-acfe-ffb9f231727f', extraction).
narrative_ontology:cs_interpretation_layer_present('7af98eee-7fea-4823-acfe-ffb9f231727f').
narrative_ontology:cs_reading_relation('7af98eee-7fea-4823-acfe-ffb9f231727f', article_9_renunciation__absolute_pacifism_reading, coexists_with).
narrative_ontology:cs_reading_relation('7af98eee-7fea-4823-acfe-ffb9f231727f', article_9_renunciation__reinterpretation_2014_reading, influences).
narrative_ontology:cs_axiom('7af98eee-7fea-4823-acfe-ffb9f231727f', foundational, inherent_right_of_self_defense_survives_text).
narrative_ontology:cs_axiom_status(inherent_right_of_self_defense_survives_text, holdable).
narrative_ontology:cs_axiom_grounding('7af98eee-7fea-4823-acfe-ffb9f231727f', inherent_right_of_self_defense_survives_text, deontological).
narrative_ontology:cs_axiom('7af98eee-7fea-4823-acfe-ffb9f231727f', foundational, war_potential_distinguishable_from_defense_potential).
narrative_ontology:cs_axiom_status(war_potential_distinguishable_from_defense_potential, holdable).
narrative_ontology:cs_axiom_grounding('7af98eee-7fea-4823-acfe-ffb9f231727f', war_potential_distinguishable_from_defense_potential, instrumental).
narrative_ontology:cs_reference_frame('7af98eee-7fea-4823-acfe-ffb9f231727f', constrained_defense_posture).
narrative_ontology:cs_drift_state('7af98eee-7fea-4823-acfe-ffb9f231727f', contemporary_post_2014_expansion, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('7af98eee-7fea-4823-acfe-ffb9f231727f', '').
narrative_ontology:cs_kernel_id(article_9_renunciation__self_defense_interpretation_reading, article_9_renunciation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_9_renunciation__self_defense_interpretation_reading, sdf_legality).
narrative_ontology:constraint_beneficiary(article_9_renunciation__self_defense_interpretation_reading, us_japan_alliance).
narrative_ontology:constraint_victim(article_9_renunciation__self_defense_interpretation_reading, literalist_pacifism_doctrine).
narrative_ontology:constraint_victim(article_9_renunciation__self_defense_interpretation_reading, article_9_plain_text_authority).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITERALIST PACIFISM (SNARE) — The plain text of Article 9 renounces war potential absolutely. This reading is trapped by the judicial doctrine that constitutional text must be read as written; yet the doctrine itself is suppressed by the political settlement that the SDF exists. The literalist position cannot exit: reading Article 9 plainly is now characterized as textually naive, and advocating for actual disarmament is politically isolated. Maximum extraction — the text is nominally supreme but its plain meaning has been overridden by administrative reading without formal amendment. No organized exit pathway exists for literalist interpretation.
constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CONSTITUTIONAL LEGALISTS (TANGLED ROPE) — Those who believe Article 9 should be formally amended via Article 96 process experience both coordination and extraction. Coordination: the self-defense interpretation does clarify that Japan has retained a legitimate right under international law. Extraction: the interpretation bypasses the amendment process that legalists believe is constitutionally required. Constrained exit: formal amendment is legally available but politically prohibitive (requires supermajority and public referendum). The legalist position benefits from the stability the self-defense interpretation provides (avoiding the SDF's unconstitutionality accusation) while bearing the cost of constitutional bypass.
constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: SDF INSTITUTIONAL LEGITIMACY (ROPE) — The self-defense interpretation directly coordinates the SDF's institutional existence with Article 9. The executive can maintain the SDF as constitutionally legitimate while keeping Article 9 formally unchanged. This is net coordination: the interpretation solves the legal problem of the SDF's status. The SDF can exit this interpretation frame via formal amendment, but has no incentive to (arbitrage position). The executive experiences this interpretation as the mechanism that resolves the legal tension between renouncing war potential and needing defense forces.
constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: US-JAPAN ALLIANCE (ROPE) — The self-defense interpretation enables Japan to maintain meaningful defense capacity while preserving Article 9 as a constraint on militarism. This is pure coordination from the alliance perspective: the interpretation allows both partners to treat Japan as a legitimate regional security actor while respecting the postwar pacifist constitution as formally intact. The alliance has arbitrage options (the US could reposition or redefine regional strategy) but the self-defense interpretation is the mechanism that coordinates Japan's defense capability with its constitutional constraints. Net beneficiary — the interpretation legitimizes Japan's security role.
constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: CONSTITUTIONAL FORMALISM (PITON) — The Japanese judiciary has deferred to the executive's self-defense reading as a political question beyond judicial review. This deference is substantially performative: the courts declare they will not intervene in security decisions while nominally interpreting Article 9 as binding. The theater ratio is high (courts issue solemn pronouncements on constitutionality while disclaiming authority to enforce them), and the function is degraded (constitutional review is theatrical when the outcome is predetermined). The judicial formalism persists through institutional inertia — courts maintain the appearance of constitutional guardianship while ceding actual interpretation to the executive. This is piton: a former coordination mechanism (judicial review as a genuine constitutional check) now substantially theatrical.
constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 6: PACIFIST CIVIL SOCIETY (TANGLED ROPE) — The peace movement and pacifist advocacy groups experience genuine coordination (the self-defense interpretation does establish that Article 9 imposes some constraint on militarism — it prohibits offensive war and collective defense overseas — distinguishing Japan from unrestricted rearmament). But they also bear extraction: their interpretation of Article 9 as an absolute pacifist commitment is suppressed by the self-defense reading, and they have constrained exit (they can advocate for formal amendment but lack political power to achieve it). The movement benefits from the constraint itself (Article 9 is real, even narrowed) while being victimized by the interpretation that narrows it.
constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the inherent right of self-defense is a pre-textual right that cannot be renounced — all states retain this right under international law regardless of their constitutional text. No written text can override the state's fundamental right to exist and defend itself. This perspective treats the self-defense interpretation as disclosing an immutable fact: Article 9 could never have meant absolute disarmament because that would violate the state's inherent right. From this view, the interpretation simply makes explicit what was always true. However, this mountain classification is a false summit: the 'inherent right' is itself a doctrinal claim that benefits specific institutional arrangements (the SDF's existence, the alliance's architecture) and is advanced by beneficiaries. The engine will detect this as a false-summit candidate.
constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_9_renunciation__self_defense_interpretation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(article_9_renunciation__self_defense_interpretation_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(article_9_renunciation__self_defense_interpretation_reading, TR),
    TR >= 0.70.

:- end_tests(article_9_renunciation__self_defense_interpretation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38): Moderate. The interpretation creates an asymmetry between the beneficiaries (SDF legality, alliance capability) and the victims (literalist reading of Article 9, pacifist constraint doctrine). The beneficiaries gain institutional authority to define what 'war potential' means and can expand force levels within the 'minimum necessary' frame. The victims lose authority over Article 9's plain meaning. However, the extraction is constrained by the interpretation's own limiting structure: it does impose constraints on offensive doctrine and force levels, distinguishing Japan from unrestricted rearmament. Suppression (0.48): Moderate. The interpretation suppresses the literalist reading by rendering it politically isolated and characterizing it as constitutionally naive. Yet the suppression is not total — the peace movement and legalist scholars maintain continuous advocacy for literal interpretation or formal amendment. The judiciary's deference to the executive provides institutional suppression but without violence or explicit prohibition. Theater ratio (0.62): Moderate-high. The judicial system's pronouncements on Article 9 are substantially performative: courts declare they will not intervene in security decisions while solemnly interpreting the constitution. The constitutional formalism of treating Article 9 as binding while ceding its interpretation to the executive creates theater. The theater has increased over time as the gap between formal constitutional text and actual practice has widened. Measurement trajectory: extractiveness rises from 0.22 (1949, right after the constitution's adoption, when pacifism was genuinely political consensus) to 0.38 (present, as the interpretation has been institutionalized and force levels have gradually expanded within the 'minimum necessary' frame). Suppression rises slightly (0.42 to 0.48) as literalist alternatives become more marginal. Theater increases (0.48 to 0.62) as the judiciary's deference becomes more pronounced and the gap between text and practice more visible.
 *
 * PERSPECTIVAL GAP:
 *   This constraint displays the full range of DR classifications from a single normative base. The literalist reading sees a snare: the text is suppressed and cannot defend itself. The institutional beneficiaries (SDF, alliance) see a rope: the interpretation solves their coordination problem. The constitutional legalists see tangled rope: they gain institutional stability but lose amendment authority. The pacifist movement sees tangled rope: they retain partial constraint but lose their authoritative reading. The judicial system sees a piton: they perform constitutional review while deferring substance to the executive. The analytical observer risks a mountain: naturalizing the inherent right of self-defense as pre-textual law. The engine's false summit detector will flag this gap: if the 'inherent right' is genuinely pre-textual and universal, why are the beneficiaries concentrated among those with institutional power over security policy? The false summit reveals the interpretation as a doctrinal choice that benefits specific institutional actors, not a natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's experienced extractiveness depends on the agent's position relative to the interpretation's operation. The SDF and alliance benefit directly from the interpretation's legitimization — they have arbitrage options (they could seek formal amendment, exit the interpretation frame) but face no incentive to do so. They derive low or negative d values (they are beneficiaries with exit capability). The literalist reading and pacifist movement are trapped by the interpretation — they have no effective exit option short of building a political coalition for constitutional amendment or judicial reversal, both of which face prohibitive costs. They derive high d values (they are victims without exit). Constitutional legalists and civil society are constrained — they can advocate for amendment or judicial reversal but face high political costs. They derive moderate-to-high d values reflecting their constrained position. The judicial system has arbitrage-like positioning (it can defer or intervene) but chooses deference, making the piton classification appropriate: institutional actors with genuine choice (arbitrage) but choosing performative inaction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    inherent_right_doctrinal_status,
    'Is the ''inherent right of self-defense'' a pre-textual universal principle that survives any constitutional text, or is it a doctrinal construction that benefits the self-defense interpretation''s beneficiaries?',
    'Comparative constitutional analysis: examination of other pacifist or explicitly disarmed constitutions (Costa Rica, Panama) and whether international law treats them as implicitly retaining self-defense rights despite their texts; analysis of which states advance the ''inherent right'' doctrine and in what contexts.',
    'If inherent right is universal and pre-textual: the mountain perspective is correct, and the interpretation simply discloses existing law — extractiveness drops to near-zero (natural constraint). If inherent right is a doctrinal choice: the mountain is a false summit, and the interpretation is an institutionalized extraction mechanism with extractiveness ~0.38 (tangled rope). This is the core mandatrophy for this constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(inherent_right_doctrinal_status, conceptual, 'Whether the inherent right of self-defense is pre-textual or doctrinal').

omega_variable(
    minimum_necessary_forces_definition,
    'What constitutes ''minimum necessary forces for defense''? Is this a self-limiting principle, or has it become a vehicle for justified expansion toward peer-competitor capability?',
    'Longitudinal budget and capability analysis of SDF forces relative to stated defense postures (regional denial vs. territorial defense); tracking of policy documents and white papers claiming ''minimum'' status for each new capability acquisition (submarine fleets, missile systems, aircraft carriers); comparative analysis with similarly-situated democracies'' defense spending.',
    'If ''minimum'' remains genuinely limiting: suppression is stable and extraction is constrained. If ''minimum'' has drifted toward peer-level capability building: suppression of offensive doctrine persists while suppression of quantitative expansion has collapsed — extractiveness may rise toward 0.55+ (snare territory).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(minimum_necessary_forces_definition, empirical, 'Whether minimum necessary forces definition remains constraining or has drifted').

omega_variable(
    id_2014_reinterpretation_precedent,
    'Does the 2014 reinterpretation that approved collective self-defense constitute a reading of the self-defense interpretation, or a qualitative break that forecloses it?',
    'Textual analysis of 2014 cabinet decision and subsequent defense white papers: does the 2014 reinterpretation claim to derive from the inherent right to self-defense (continuous with this reading), or does it introduce new doctrinal ground (foreclosing the earlier reading''s limiting structure)?',
    'If continuous: the self-defense interpretation reading remains coherent and the 2014 reinterpretation is a sibling reading that coexists with it. If discontinuous: the 2014 reinterpretation may have functionally foreclosed the ''minimum necessary'' limiting principle, collapsing the tangled_rope into snare territory. This affects both the reading''s own internal logic and its relationship to the absolute_pacifism_reading (which sees both reinterpretations as violations of the same principle).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(id_2014_reinterpretation_precedent, conceptual, 'Whether 2014 reinterpretation is continuous with or forecloses the self-defense interpretation').

omega_variable(
    constitutional_amendment_availability,
    'Why has formal amendment via Article 96 never been attempted for Article 9, despite seventy years of the SDF''s existence?',
    'Political history analysis: examination of elite cost-benefit calculations regarding Article 96 amendment (supermajority + referendum), public opinion tracking on pacifism, institutional path-dependency of the current interpretation vs. amendment route. Counterfactual: what would successful amendment mean for regional stability, alliance credibility, domestic legitimacy?',
    'If amendment is avoided because the interpretation serves all stakeholders better (lower political cost, preserves pacifist legitimacy while enabling defense): the tangled_rope classification is stable. If amendment is avoided because pacifism has become genuinely immovable despite losing substantive meaning: the constraint may have shifted to piton (performative constitutional commitment) or rope (pure coordination around maintaining the status quo). This affects the constitutional legalist perspective''s exit options.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(constitutional_amendment_availability, preference, 'Why Article 96 formal amendment has never been attempted').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_9_renunciation__self_defense_interpretation_reading, 0, 75).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t0, article_9_renunciation__self_defense_interpretation_reading, theater_ratio, 0, 0.48).
narrative_ontology:measurement(arti_tr_t25, article_9_renunciation__self_defense_interpretation_reading, theater_ratio, 25, 0.55).
narrative_ontology:measurement(arti_tr_t50, article_9_renunciation__self_defense_interpretation_reading, theater_ratio, 50, 0.62).

% Extraction over time
narrative_ontology:measurement(arti_be_t0, article_9_renunciation__self_defense_interpretation_reading, base_extractiveness, 0, 0.22).
narrative_ontology:measurement(arti_be_t25, article_9_renunciation__self_defense_interpretation_reading, base_extractiveness, 25, 0.31).
narrative_ontology:measurement(arti_be_t50, article_9_renunciation__self_defense_interpretation_reading, base_extractiveness, 50, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t0, article_9_renunciation__self_defense_interpretation_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(arti_su_t25, article_9_renunciation__self_defense_interpretation_reading, suppression_requirement, 25, 0.45).
narrative_ontology:measurement(arti_su_t50, article_9_renunciation__self_defense_interpretation_reading, suppression_requirement, 50, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_9_renunciation__self_defense_interpretation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_9_renunciation__self_defense_interpretation_reading, article_9_renunciation__absolute_pacifism_reading).
narrative_ontology:affects_constraint(article_9_renunciation__self_defense_interpretation_reading, article_9_renunciation__reinterpretation_2014_reading).

% DUAL FORMULATION NOTE:
% Article 9's renunciation is the kernel, shared across three constraint stories representing three different readings. Each reading instantiates the kernel differently, yielding different ε values, beneficiary/victim structures, and classifications. The self-defense interpretation reading (ε=0.38, tangled_rope) interprets Article 9 as renouncing aggressive war while preserving inherent self-defense rights. The absolute_pacifism_reading (expected ε≤0.25, mountain or rope) interprets Article 9 as absolute renunciation, making the SDF unconstitutional. The reinterpretation_2014_reading (expected ε≥0.45, snare or tangled_rope) treats the 2014 collective defense authorization as a violation of the limiting structure. These are not observable-dependent versions of one constraint — they are structurally distinct readings with different beneficiary/victim structures, different suppression mechanisms, and different extraction profiles. They are linked by network effects: the self-defense interpretation enables the 2014 reinterpretation, which in turn pressures the self-defense interpretation's limiting structure. Each story must be authored separately; the kernel context identifies the shared doctrinal dispute.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
