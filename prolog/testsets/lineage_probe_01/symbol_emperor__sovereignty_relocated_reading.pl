% ============================================================================
% CONSTRAINT STORY: symbol_emperor__sovereignty_relocated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_symbol_emperor_sovereignty_relocated_reading, []).

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
 *   constraint_id: symbol_emperor__sovereignty_relocated_reading
 *   human_readable: Symbol Emperor Clause: Sovereignty Relocated from Emperor to People (Reading)
 *   domain: legal/constitutional/doctrinal
 *
 * SUMMARY:
 *   The symbol clause in the 1947 postwar Japanese Constitution (Article 1)
 *   accomplishes a singular feat: it relocates sovereignty from emperor to
 *   people in a single sentence, using the gentlest possible language. The
 *   clause reads: 'The Emperor shall be the symbol of the State and of the
 *   unity of the people, deriving his position from the will of the people
 *   with whom resides sovereignty.' This constraint represents one reading of
 *   a deeply contested constitutional kernel — the symbol emperor itself. The
 *   sovereignty_relocated_reading instantiates the thesis that this clause is
 *   THE revolution: that the deepest structural change in the document (the
 *   transfer of sovereign authority from an emperor understood as divinely
 *   descended to a people understood as the new source of legitimacy) is
 *   accomplished through the most restrained and non-revolutionary language.
 *   The clause does not declare 'the emperor is dead' or 'divine descent is
 *   abolished' — it simply relocates sovereignty and re-derives the imperial
 *   position from the will of the people. The magic is in what the sentence
 *   does not need to say. This reading coexists with two sibling readings:
 *   the continuity_device_reading (which frames the symbol emperor as the
 *   occupation's pragmatic bargain — preserve the throne to make the new
 *   order governable) and the kokutai_severed_reading (which emphasizes that
 *   the clause killed kokutai as law — the mystical national polity was
 *   demoted to ceremony, not retained as a governing principle). The
 *   sovereignty_relocated_reading focuses on the relocation of legitimacy
 *   itself — the structural fact that authority now derives 'from below' (the
 *   will of the people) rather than 'from above' (divine descent through the
 *   imperial line).
 *
 * KEY AGENTS:
 *   - The Sovereign People: Newly named source of authority; beneficiary of the relocation but constrained by the symbol's persistence (moderate/constrained)
 *   - Imperial Authority Structures: Pre-war legitimacy frame grounded in divine descent; victim of the severance (powerless/trapped)
 *   - Divinely-Descended Rule Principle: The theological/metaphysical claim that authority flows from heaven through the imperial line; victim of relocation (trapped in the legal order, though may persist extra-legally)
 *   - Postwar Democratic Institutions: Organized agents implementing the new constitutional order; benefit from the relocation and the coordination function provided by symbol retention (organized/constrained)
 *   - Occupying Authority (Allied Occupation): Institutional actor enforcing the new order; has arbitrage — can exit after establishing the transition (institutional/arbitrage)
 *   - The Symbol Emperor Itself: The literal text and ceremonial form; persists as a coordination mechanism and as a source of tension within the new order
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(symbol_emperor__sovereignty_relocated_reading, 0.18).
domain_priors:suppression_score(symbol_emperor__sovereignty_relocated_reading, 0.62).
domain_priors:theater_ratio(symbol_emperor__sovereignty_relocated_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(symbol_emperor__sovereignty_relocated_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(symbol_emperor__sovereignty_relocated_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(symbol_emperor__sovereignty_relocated_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(symbol_emperor__sovereignty_relocated_reading, tangled_rope).
narrative_ontology:human_readable(symbol_emperor__sovereignty_relocated_reading, "Symbol Emperor Clause: Sovereignty Relocated from Emperor to People (Reading)").
narrative_ontology:topic_domain(symbol_emperor__sovereignty_relocated_reading, "legal/constitutional/doctrinal").

domain_priors:requires_active_enforcement(symbol_emperor__sovereignty_relocated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(symbol_emperor__sovereignty_relocated_reading, '284f8a95-decd-43e8-ab7a-bde0e453e9ba').
narrative_ontology:cs_kernel_codification('284f8a95-decd-43e8-ab7a-bde0e453e9ba', fixed_text).
narrative_ontology:cs_authority_grounding('284f8a95-decd-43e8-ab7a-bde0e453e9ba', extraction).
narrative_ontology:cs_interpretation_layer_present('284f8a95-decd-43e8-ab7a-bde0e453e9ba').
narrative_ontology:cs_reading_relation('284f8a95-decd-43e8-ab7a-bde0e453e9ba', symbol_emperor__symbol_emperor_continuity_device_reading, coexists_with).
narrative_ontology:cs_reading_relation('284f8a95-decd-43e8-ab7a-bde0e453e9ba', symbol_emperor__symbol_emperor_kokutai_severed_reading, influences).
narrative_ontology:cs_axiom('284f8a95-decd-43e8-ab7a-bde0e453e9ba', foundational, sovereignty_relocates_from_above_to_below).
narrative_ontology:cs_axiom_status(sovereignty_relocates_from_above_to_below, holdable).
narrative_ontology:cs_axiom_grounding('284f8a95-decd-43e8-ab7a-bde0e453e9ba', sovereignty_relocates_from_above_to_below, deontological).
narrative_ontology:cs_axiom('284f8a95-decd-43e8-ab7a-bde0e453e9ba', foundational, imperial_position_re_derived_from_popular_will).
narrative_ontology:cs_axiom_status(imperial_position_re_derived_from_popular_will, holdable).
narrative_ontology:cs_axiom_grounding('284f8a95-decd-43e8-ab7a-bde0e453e9ba', imperial_position_re_derived_from_popular_will, conventional).
narrative_ontology:cs_reference_frame('284f8a95-decd-43e8-ab7a-bde0e453e9ba', divine_descent_imperial_sovereignty).
narrative_ontology:cs_drift_state('284f8a95-decd-43e8-ab7a-bde0e453e9ba', postwar_constitutional_era, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('284f8a95-decd-43e8-ab7a-bde0e453e9ba', '').
narrative_ontology:cs_kernel_id(symbol_emperor__sovereignty_relocated_reading, symbol_emperor).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(symbol_emperor__sovereignty_relocated_reading, sovereign_people).
narrative_ontology:constraint_beneficiary(symbol_emperor__sovereignty_relocated_reading, postwar_democratic_institutions).
narrative_ontology:constraint_victim(symbol_emperor__sovereignty_relocated_reading, imperial_authority_structures).
narrative_ontology:constraint_victim(symbol_emperor__sovereignty_relocated_reading, divinely_descended_rule_principle).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: IMPERIAL AUTHORITY STRUCTURES (SNARE) — The symbol clause removes the structural foundation (divine descent, imperial sovereignty) upon which rule-in-the-emperor's-name was constituted. These structures are trapped: they cannot exit the new constitutional order, cannot reorganize under the old legitimacy, cannot operate as though the relocation of sovereignty has not occurred. The extraction from this perspective is maximal and irreversible — the very ground of imperial authority has been rewritten in a single sentence.
constraint_indexing:constraint_classification(symbol_emperor__sovereignty_relocated_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: POSTWAR DEMOCRATIC INSTITUTIONS (TANGLED ROPE) — The symbol clause creates a genuine coordination function: it enables the new democratic order to govern while preserving the ceremonial and unifying role of the emperor. The occupation (and later, independent Japanese government) benefits from this arrangement — it provides a stable transition mechanism and prevents the destabilizing claim that the new constitution is a complete rupture. But this constraint also extracts from democratic legitimacy: the emperor's continued symbolic role is a residual imperial authority that sits uneasily within a system that claims sovereignty derives from the people. The tension is active and ongoing — enforced through constitutional interpretation and ceremonial boundary-maintenance.
constraint_indexing:constraint_classification(symbol_emperor__sovereignty_relocated_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: OCCUPYING AUTHORITY & CONSTITUTIONAL ARCHITECTS (ROPE) — From the occupation's perspective, the symbol clause is a pure coordination mechanism. It solves the governance problem: how to introduce democratic sovereignty while retaining institutional stability. The occupation has arbitrage — it can enforce this reading and then exit, leaving the new order to self-sustain. The coordination benefit is immediate: the throne survives, the people become sovereign, no civil war erupts. Extraction is minimal from this perspective because the occupation has no long-term stake in the constraint — it is a temporary mechanism for transition.
constraint_indexing:constraint_classification(symbol_emperor__sovereignty_relocated_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE SOVEREIGN PEOPLE (TANGLED ROPE) — The symbol clause names 'the people' as the source of sovereignty — a profound benefit. But the people are constrained by the symbolic residue: the emperor is retained, the imperial mystique persists, and the exact boundaries of popular sovereignty are ambiguous (How far does popular sovereignty extend when ceremony and symbol remain imperial?). The people benefit from the named sovereignty but are constrained in exercising it by the unresolved question of what the symbol emperor's retained role means for democratic authority. This is genuine mixed coordination-extraction: the clause both names and limits popular sovereignty simultaneously.
constraint_indexing:constraint_classification(symbol_emperor__sovereignty_relocated_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER / CONTINUITY FRAME (MOUNTAIN) — From a civilizational/universal perspective, one might argue that the symbol emperor clause merely formalizes what was always true: the emperor was always a symbol, always wielded power through delegated authority, always depended on ministers and institutions. The clause, in this reading, reveals rather than invents popular sovereignty — it exposes the structure that was always there. This perspective sees the clause as discovering an immutable truth about political authority: sovereignty can be re-rooted without breaking institutional continuity. However, this reading naturalizes what is structurally a radical relocation. The engine will identify this as a false summit — continuity language masks a decisive severing of the legitimacy principle.
constraint_indexing:constraint_classification(symbol_emperor__sovereignty_relocated_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 6: LITERAL TEXT OF THE SYMBOL CLAUSE (PITON) — The clause itself reads: 'The Emperor shall be the symbol of the State and of the unity of the people, deriving his position from the will of the people with whom resides sovereignty.' The literal words are gentle, performative in their restraint. The clause names the people as sovereign but continues to name the emperor. The magic is in what is omitted: the sentence does not say 'the emperor no longer has sovereignty' or 'divine descent is abolished.' It simply relocates sovereignty and re-derives imperial position from the will of the people. The theater here (0.55) reflects that the clause performs a revolution through ceremony and indirection — the deepest change wearing the gentlest words. The theater_ratio captures that much of the clause's force is in what it does not need to say, making the revision itself somewhat theatrical (the appearance of continuity masking radical change).
constraint_indexing:constraint_classification(symbol_emperor__sovereignty_relocated_reading, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(symbol_emperor__sovereignty_relocated_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(symbol_emperor__sovereignty_relocated_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(symbol_emperor__sovereignty_relocated_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(symbol_emperor__sovereignty_relocated_reading, TR),
    TR >= 0.70.

:- end_tests(symbol_emperor__sovereignty_relocated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. This reading measures extractiveness as the severance cost borne by imperial legitimacy structures, not as ongoing extraction from the newly sovereign people. The low value reflects that the clause, once established and internalized, does not require continuous extraction to maintain — the relocation is accomplished once and then becomes the new constitutional baseline. Over the interval, extractiveness declines (0.28 → 0.18) as the new order settles and the structural change becomes normalized. Suppression (0.62): High. The suppression required to prevent the re-assertion of imperial sovereignty (and the theological claims that underlie it) is substantial. The clause suppresses alternative readings of legitimacy (divinity, descent) and prevents the old principle from organizing rule. Suppression remains high because imperial traditionalism and kokutai sentiment never fully disappeared — they were suppressed into ceremonial and cultural registers outside the legal order. Theater ratio (0.55): Moderate-high. The clause accomplishes a revolution through indirection. The theater reflects that the deepest change is performed through the gentlest words — the magic is in what is omitted and what is implicit. The clause does not perform a violent revolution; it performs a legal relocation through the ceremonial power of constitutional language. Over the interval, theater_ratio rises slightly (0.38 → 0.55) as the clause becomes established and its force is increasingly performed through absence (the unspoken severing) rather than through explicit statements. The suppression requirement declines (0.72 → 0.62) as enforcement of the new order becomes normalized — active suppression gives way to routine constitutional interpretation.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits maximal perspectival gap. The imperial authority structures perceive snare (extraction without exit). The occupying authority perceives rope (pure coordination). The democratic institutions perceive tangled rope (mixed benefit and constraint). The sovereign people perceive tangled rope from below (benefit named, exit constrained). The symbol text itself reads as piton (a revolution performed through ceremonial restraint, with high theater). The analytical observer risks perceiving mountain (the relocation of sovereignty is treated as discovering a universal truth about political authority). These gaps are not disagreements about facts — they all observe the same clause — but rather differences in structural position that generate genuinely different classifications from the same constraint. The gap reveals that the constraint's force is distributed and relational, not absolute.
 *
 * DIRECTIONALITY LOGIC:
 *   The sovereignty_relocated_reading declares the sovereign people as beneficiaries and imperial authority structures as victims. The directionality computation derives d from these structural positions: the people gain the named source of authority (beneficiary status, d low), but their exit options are constrained (they cannot exit the new constitutional order without a new constitutional change — constitutional amendment requires broad consensus, making their mobility 'constrained' rather than 'mobile'). This produces moderate d, and with moderate power (representing distributed agency across a dispersed population), the people experience moderate effective extraction despite their beneficiary status — their sovereignty is named but bounded by the symbol's persistence. Imperial structures are victims facing a trapped exit (they cannot operate under the old legitimacy in the new legal order, and they cannot exit the new order without forcing a constitutional rupture), producing high d and victim-class extractiveness. The occupying authority faces low d (beneficiary status with arbitrage exit — they can establish the transition and then withdraw), producing negative or near-zero effective extraction from their perspective. The tangled rope classification at the institutional level (beneficiary democratic institutions) reflects the tension: these actors both benefit from the relocation (they now rule in the people's name) and are constrained by it (they must defer to the named source of popular sovereignty, which they do not directly control and must continually re-justify through democratic performance).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_versus_rupture_ambiguity,
    'Is the symbol clause a revelation of persistent popular sovereignty that was always true, or a radical relocation of legitimacy from a previously sovereign emperor to a newly sovereign people?',
    'Historical analysis of pre-war imperial doctrine and post-war constitutional practice. If imperial authority had always been understood as delegated by the people, the clause is continuity. If imperial authority had been understood as inherent and divine, the clause is rupture. The answer determines which reading (sovereignty_relocated_reading vs continuity_device_reading) describes structural reality versus rhetoric.',
    'If rupture: extractiveness from imperial structures is severe (ε ≈ 0.45); the clause severs the legitimacy principle itself. If continuity: extractiveness is moderate (ε ≈ 0.15); the clause formalizes what was latent. This omega determines whether the false summit mountain perspective is appropriate or misleading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_versus_rupture_ambiguity, conceptual, 'Whether the symbol clause represents a radical relocation of sovereignty or a formalization of always-present popular sovereignty').

omega_variable(
    kokutai_status_after_clause,
    'Does the symbol clause abolish kokutai (the sacred national polity grounded in imperial divinity) as law, or does it merely demote kokutai to ceremonial status while preserving it as a cultural and religious claim outside the legal order?',
    'Textual analysis of post-war constitutional interpretation, imperial rescript language, and the scope of the clause''s legal authority. If kokutai remains as a competing claim to legitimacy (held by traditionalists, religious actors, segments of the military), the clause has not abolished it — only narrowed its legal scope. If kokutai has been formally and completely severed, a different reading applies.',
    'If kokutai persists as extra-legal legitimacy claim: the symbol clause is incomplete, and suppression of the old principle (0.62) may be too high. If kokutai is truly severed: the suppression figure and the beneficiary claim (sovereign people) are accurate. This omega distinguishes the sovereignty_relocated_reading from the kokutai_severed_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kokutai_status_after_clause, empirical, 'Whether kokutai is abolished as law or merely demoted to ceremonial/cultural status').

omega_variable(
    extractiveness_direction_ambiguity,
    'Who bears the extractive cost of the symbol clause — the imperial structures whose legitimacy is relocated, or the newly sovereign people who are named sovereign but constrained by the symbolic residue?',
    'Longitudinal analysis of power redistribution post-clause: Have imperial structures lost the ability to rule in their own name? Have the people exercised autonomous sovereignty, or have they been constrained by deference to the symbol? Which group has had to adjust more significantly?',
    'If imperial structures bear cost: extractiveness reflects the severance of their legitimacy (ε ≈ 0.45, victims = imperial authority structures). If the people bear cost: extractiveness reflects their constrained sovereignty (ε ≈ 0.35, victims = the nominal beneficiaries who cannot exercise full authority). The reading emphasizes the former, but the ambiguity is real.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractiveness_direction_ambiguity, empirical, 'Whether extractiveness flows from severance of imperial legitimacy or constraint on popular sovereignty').

omega_variable(
    authority_grounding_after_relocation,
    'After the symbol clause relocates sovereignty to the people, from what source does the people''s own authority derive? Is it self-evident (the people just have it), grounded in natural law, grounded in popular consent (circular), or grounded in the occupation authority''s enforcement?',
    'Constitutional text, preamble analysis, and interpretive doctrine. The postwar constitution''s preamble asserts that government power is grounded in ''We the people,'' but the people''s own authority to grant this power to the state is not itself specified. If it is treated as self-evident, the reading is coherent. If it is understood as grounded in the occupation''s enforcement, the reading becomes more complex — the people are sovereign by decree of a foreign power, which is a structural contradiction.',
    'If people''s authority is self-evident: the symbol clause fully succeeds in relocating sovereignty. If people''s authority is grounded in occupation enforcement: the reading describes a temporary transfer, and the true sovereign (the occupation) has merely delegated authority to the people, making this a tangled_rope from the occupation''s perspective but a snare from the people''s perspective (they are sovereign only by the occupier''s will).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authority_grounding_after_relocation, conceptual, 'Source of the people''s own authority after the symbol clause').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(symbol_emperor__sovereignty_relocated_reading, 0, 5).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(symempsov_theater_t0, symbol_emperor__sovereignty_relocated_reading, theater_ratio, 0, 0.38).
narrative_ontology:measurement(symempsov_theater_t2, symbol_emperor__sovereignty_relocated_reading, theater_ratio, 2, 0.48).
narrative_ontology:measurement(symempsov_theater_t5, symbol_emperor__sovereignty_relocated_reading, theater_ratio, 5, 0.55).

% Extraction over time
narrative_ontology:measurement(symempsov_extract_t0, symbol_emperor__sovereignty_relocated_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(symempsov_extract_t2, symbol_emperor__sovereignty_relocated_reading, base_extractiveness, 2, 0.22).
narrative_ontology:measurement(symempsov_extract_t5, symbol_emperor__sovereignty_relocated_reading, base_extractiveness, 5, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(symempsov_suppress_t0, symbol_emperor__sovereignty_relocated_reading, suppression_requirement, 0, 0.72).
narrative_ontology:measurement(symempsov_suppress_t2, symbol_emperor__sovereignty_relocated_reading, suppression_requirement, 2, 0.68).
narrative_ontology:measurement(symempsov_suppress_t5, symbol_emperor__sovereignty_relocated_reading, suppression_requirement, 5, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(symbol_emperor__sovereignty_relocated_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(symbol_emperor__sovereignty_relocated_reading, symbol_emperor_continuity_device_reading).
narrative_ontology:affects_constraint(symbol_emperor__sovereignty_relocated_reading, symbol_emperor_kokutai_severed_reading).

% DUAL FORMULATION NOTE:
% The symbol_emperor kernel generates three constraint stories corresponding to three contested readings: (1) sovereignty_relocated_reading (ε=0.18): emphasizes the relocation of legitimacy from emperor to people; (2) continuity_device_reading (ε≈0.35): emphasizes the pragmatic preservation of the throne as a bargain; (3) kokutai_severed_reading (ε≈0.42): emphasizes the theological/metaphysical rupture (divine descent → ceremonial symbol). These are not three measurements of the same constraint — they are three different constraints arising from three different readings of the same constitutional text. Each reading assigns different beneficiary/victim structures and different interpretations of what the clause accomplishes. Linked via affects_constraints to enable contamination analysis and cross-reading validity checks.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
