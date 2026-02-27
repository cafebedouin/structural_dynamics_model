% ============================================================================
% CONSTRAINT STORY: kjv_textual_authority
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-24
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kjv_textual_authority, []).

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
 *   constraint_id: kjv_textual_authority
 *   human_readable: The King James Textual Monopoly
 *   domain: religious/linguistic/political
 *
 * SUMMARY:
 *   The King James Version, commissioned by King James I and completed in
 *   1611, became the dominant English translation of Scripture for nearly 300
 *   years. Officially authorized as the successor to the Geneva Bible (which
 *   contained politically radical marginal glosses), the KJV was designed to
 *   centralize religious authority within the Crown and the established
 *   church hierarchy. The constraint operates across multiple structural
 *   levels: (1) legal suppression of competing translations (weakest
 *   1611–1700, fading by 1800); (2) market monopoly sustained by printing
 *   privileges and institutional patronage; (3) cultural prestige and
 *   theological authority; (4) liturgical standardization. The constraint
 *   exhibits all six DR types from different perspectives. For dissenting
 *   communities and alternative translation advocates, the KJV monopoly
 *   appears as pure extraction (Snare) — they bear the costs of exclusion,
 *   suppression, and forced conformity. For the Crown and established church,
 *   it appears as pure coordination (Rope) — unifying doctrine, stabilizing
 *   liturgy, and centralizing religious authority. By the 19th century,
 *   authorized pluralism (English Revised Version, American Standard Version)
 *   created a sunset clause, and the constraint shifted toward Scaffold. By
 *   the 21st century, the KJV has become a Piton — maintained through
 *   cultural memory and ceremonial prestige rather than active enforcement.
 *   The constraint demonstrates how mandatrophy manifests: is the KJV
 *   monopoly a legitimate coordination mechanism (rope) that happened to
 *   involve extraction, or is it pure extraction (snare) disguised as
 *   coordination? The historical record suggests both: it coordinated the
 *   English church (genuine beneficiary) while extracting from dissenting
 *   communities (genuine victims). The mandatrophy is not resolved but rather
 *   shows how structural ambiguity can persist for centuries.
 *
 * KEY AGENTS:
 *   - Crown/Established Church Hierarchy: Primary beneficiary (institutional/arbitrage) — consolidates religious authority, standardizes doctrine, subordinates theological discourse
 *   - Dissenting Communities: Primary victim (powerless/trapped) — face legal suppression, market exclusion, and theological ostracism; bear full extraction cost
 *   - Independent Printers/Translators: Secondary victim (moderate/constrained) — face legal jeopardy and market constraints; also benefit from textual standardization
 *   - Protestant Reformation Coalitions: Secondary victim/agent (organized/constrained) — constrained but able to organize resistance; produce competing texts despite suppression
 *   - 19th-Century Textual Liberalization Movement: Organized agents (organized/constrained) — build institutional pathways for translation pluralism; create sunset clause
 *   - Contemporary Protestant Communities: Institutional agents (institutional/arbitrage) — now have complete freedom to choose texts; KJV persists through choice, not enforcement
 *   - Non-English Reformed Traditions: Indirect victim (moderate/mobile) — experience dominance of KJV theology rather than direct suppression; progressively escape through native translations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kjv_textual_authority, 0.52).
domain_priors:suppression_score(kjv_textual_authority, 0.68).
domain_priors:theater_ratio(kjv_textual_authority, 0.64).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kjv_textual_authority, extractiveness, 0.52).
narrative_ontology:constraint_metric(kjv_textual_authority, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(kjv_textual_authority, theater_ratio, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kjv_textual_authority, tangled_rope).
narrative_ontology:human_readable(kjv_textual_authority, "The King James Textual Monopoly").
narrative_ontology:topic_domain(kjv_textual_authority, "religious/linguistic/political").

domain_priors:requires_active_enforcement(kjv_textual_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kjv_textual_authority, crown_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(kjv_textual_authority, established_church_hierarchy).
narrative_ontology:constraint_beneficiary(kjv_textual_authority, kjv_textual_authority).
narrative_ontology:constraint_victim(kjv_textual_authority, alternative_translation_communities).
narrative_ontology:constraint_victim(kjv_textual_authority, textual_plurality).
narrative_ontology:constraint_victim(kjv_textual_authority, protestant_dissenting_traditions).
narrative_ontology:constraint_victim(kjv_textual_authority, linguistic_access).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DISSENTING CONGREGATION (SNARE) — Trapped within the KJV monopoly. To read Scripture, teach theology, or maintain religious practice within English-speaking societies requires navigating the KJV's authority. Exit options are severely constrained: producing alternative translations risks legal suppression (particularly 1611–1700s), theological ostracism, and loss of institutional access. d≈0.93, f(d)≈1.40, σ=1.1 → χ≈0.80. This perspective bears the full extraction cost of textual monopoly.
constraint_indexing:constraint_classification(kjv_textual_authority, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: NON-ENGLISH REFORMED COMMUNITIES (SNARE) — Trapped by the KJV's theological and linguistic dominance. Communities seeking reformed Protestant theology often had to work through the KJV even when native-language translations existed, due to the KJV's status as the authoritative Protestant text. d≈0.90, f(d)≈1.38, σ=1.1 → χ≈0.77. Structural entrapment through textual prestige rather than direct legal suppression.
constraint_indexing:constraint_classification(kjv_textual_authority, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: INDEPENDENT PRINTER / DISSENTING THEOLOGIAN (TANGLED ROPE) — Constrained exit: can produce alternative texts and commentary (providing real benefit to their congregation), but faces legal jeopardy, market constraints (KJV dominates printing), and theological costs (accusation of heresy). Benefits from the textual stability the KJV provides (shared reference point) while chafing against its prescriptive authority. d≈0.68, f(d)≈1.06, σ=0.95 → χ≈0.52. Mixed extraction and coordination.
constraint_indexing:constraint_classification(kjv_textual_authority, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ESTABLISHED CHURCH HIERARCHY (ROPE) — Primary beneficiary. Experiences the KJV monopoly as coordination: unified textual authority reduces doctrinal fragmentation, stabilizes liturgy, and centralizes theological discourse. Exit options are maximal (can revise, authorize new translations at will). d≈0.08, f(d)≈-0.09, σ=1.0 → χ≈-0.05. Net beneficiary; negative extraction indicates subsidy.
constraint_indexing:constraint_classification(kjv_textual_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: CROWN ECCLESIASTICAL AUTHORITY (ROPE) — Primary beneficiary and enforcer. The KJV monopoly is a pure coordination mechanism from this perspective: unifies the English church, provides political legitimacy (Scripture as authored text), and subordinates theological discourse to Crown authority. Exit options are absolute (can commission new translations or revoke authorization). d≈0.00, f(d)≈-0.20, σ=1.1 → χ≈-0.11. Net beneficiary; maximal arbitrage.
constraint_indexing:constraint_classification(kjv_textual_authority, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: PROTESTANT REFORMATION COALITIONS (TANGLED ROPE) — Organized agents (Puritan congregations, independent preachers, sectarian groups) have partial exit: able to produce competing texts (Geneva Bible commentary, marginal glosses, alternative translations) but face collective suppression and market power of the official version. Benefit from shared textual reference point; extract costs from monopolistic control. d≈0.50, f(d)≈0.65, σ=1.0 → χ≈0.34. Moderate extraction; growing agency over time.
constraint_indexing:constraint_classification(kjv_textual_authority, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: 19TH-CENTURY TEXTUAL LIBERALIZATION (SCAFFOLD) — By 1800s, new translations (Douay-Rheims updates, English Revised Version 1881) are authorized and proliferate. The original KJV monopoly has become a scaffold: still enforced as tradition, but the sunset is structural (alternative texts are now legitimate, though KJV retains prestige). d≈0.45, f(d)≈0.48, σ=1.0 → χ≈0.31. Theater_ratio declines as functional pluralism increases; extract costs decline asymptotically.
constraint_indexing:constraint_classification(kjv_textual_authority, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: KJV AS PITON (CONTEMPORARY) — By 2026, the KJV is largely inert as a functional monopoly but persists through institutional inertia and cultural/theological prestige. Many Protestant communities can and do use alternative translations without sanction. The KJV's continued authority is performative: ceremonial readings, liturgical preference, cultural memory ('the authorized version'), historical reverence. theater_ratio ≥0.70 (performative liturgy vastly outweighs functional enforcement). d≈0.08, f(d)≈-0.09, σ=1.2 → χ≈-0.07. Negative extraction because the piton is coasting on institutional subsidy, not enforcement.
constraint_indexing:constraint_classification(kjv_textual_authority, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / LINGUISTIC INVARIANCE (MOUNTAIN) — From a civilizational/universal scope, the constraint appears as an immutable feature of textual authority: any sacred text monopoly in a language community creates structural extraction (translation authority, textual stability vs. interpretive freedom). However, the empirical data (ε=0.52, suppression=0.68, theater=0.64) contradicts pure mountain classification. The engine will detect a false summit: the KJV monopoly is a contingent political/institutional arrangement, not a law of language or interpretation.
constraint_indexing:constraint_classification(kjv_textual_authority, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kjv_textual_authority_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(kjv_textual_authority, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(kjv_textual_authority, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(kjv_textual_authority, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(kjv_textual_authority, TR),
    TR >= 0.70.

:- end_tests(kjv_textual_authority_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high at origin, declining. At t=0 (1611), the KJV monopoly extracted heavily: it displaced competing translations (particularly the Geneva Bible, which had theological authority and popular support), suppressed unauthorized translations, and concentrated textual authority in Crown hands. The extraction value begins high (≈0.72) because enforcement was active and escape routes were minimal. By t=200 (1811), some liberalization has begun (Douay-Rheims English Catholic translation authorized 1750, standard English Revised Version authorized 1881), and extraction declines to 0.58. By t=415 (2026), the monopoly is substantially relaxed (multiple translations coexist, KJV is no longer legally enforced), extractiveness is 0.52 — reflecting the residual prestige and cultural authority of the KJV even when enforcement is gone. Suppression (0.68): High and persistent. Active legal suppression declined after 1750 but cultural suppression (theological ostracism, market control, prestige barriers) remained strong through the 19th century. Contemporary suppression is low for English-speaking communities (anyone can publish alternative translations) but remains higher in conservative theological communities that discourage deviation from KJV. The 0.68 value reflects the historical average across the full interval. Theater_ratio (0.64): Moderate-high, increasing. Early in the interval (1611–1700), the KJV monopoly had genuine functional enforcement — it coordinated a fractured church, actually suppressed doctrinal chaos, and had real institutional teeth. Theater was lower (≈0.48) because the coordination function was authentic. By the 19th century, alternative texts were proliferating but the KJV ritual persisted, suggesting increasing theater (≈0.60). By 2026, the KJV's enforcement is almost entirely ceremonial (readings in liturgy, cultural prestige, theological nostalgia) while functional pluralism has replaced it. Theater has increased to 0.64 because the constraint persists through performative authority rather than structural necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint exhibits severe perspectival divergence across power positions. The Crown/established church sees a pure coordination mechanism (Rope) — the KJV unified doctrine and centralized authority. Dissenting communities see pure extraction (Snare) — they could not escape the monopoly without legal jeopardy or theological ostracism. Organized reform coalitions see mixed extraction and coordination (Tangled Rope) — they benefit from shared textual reference but chafe against exclusionary authority. By the 19th century, liberalization movements see a temporary problem with a sunset (Scaffold) — alternative translations are becoming legitimate, the monopoly is weakening, and plural authority is emerging. By the 21st century, the KJV is a Piton — maintained through institutional inertia and ceremonial prestige even though it no longer functions as a monopoly. The analytical observer risks seeing a mountain (immutable feature of linguistic/theological authority) but the empirical data shows a contingent institutional arrangement.
 *
 * DIRECTIONALITY LOGIC:
 *   Crown Ecclesiastical Authority: Beneficiary + arbitrage → d≈0.00, f(d)≈-0.20. Maximum beneficiary; can authorize or revoke at will. Established Church Hierarchy: Beneficiary + arbitrage → d≈0.08, f(d)≈-0.09. Strong beneficiary; high arbitrage. Dissenting Communities: Victim + trapped → d≈0.93, f(d)≈1.40. Maximum extraction; exit routes blocked. Independent Printers/Theologians: Victim + constrained → d≈0.68, f(d)≈1.06. High extraction; some exit possibility but with costs. Protestant Coalitions (post-1650): Victim + constrained, but organized → d≈0.50, f(d)≈0.65. Moderate extraction; growing agency. Textual Liberalization Movement (1800s): Organized + constrained → d≈0.45, f(d)≈0.48. Low-moderate extraction; clear exit path emerging. Contemporary KJV Communities: Institutional + arbitrage → d≈0.08, f(d)≈-0.09. No extraction; KJV maintained by choice. Non-English Reformed: Victim + mobile → d≈0.55, f(d)≈0.75. Moderate extraction; progressively escaping via native translations.
 *
 * MANDATROPHY ANALYSIS:
 *   UNRESOLVED MANDATROPHY: The constraint presents a classical mandatrophy case that persists unresolved. The question is: does the KJV monopoly provide genuine coordination benefit (unified doctrine, liturgical stability, centralized authority — legitimate rope function) or is the coordination benefit merely a rationalization for extraction (concentrating textual control, suppressing dissent, enforcing theological conformity — pure snare)? The historical evidence supports BOTH: (1) The Crown genuinely solved a coordination problem — the Geneva Bible's marginal glosses had created doctrinal fragmentation and political radicalism. The KJV unified the church. Coordination benefit was real. (2) The Crown also genuinely extracted — it displaced competing texts, suppressed alternatives, and concentrated religious authority. Extraction was structural. The mandatrophy cannot be resolved by declaring one or the other because both operated simultaneously. The benef beneficiary (Crown/hierarchy) derived real coordination value. The victim (dissenting communities) bore real extraction costs. The constraint is a Tangled Rope: it had both coordination and asymmetric extraction. However, the contemporary period (Piton perspective) shows that the coordination function has degraded — modern Protestant communities maintain doctrinal coherence without the KJV monopoly. This suggests the mandatrophy can be partially resolved historically: the coordination benefit, though real in 1611–1750, was not inherent to unified translation. It was institutional enforcement that provided the benefit. Once enforcement was relaxed (post-1800), equivalent coordination emerged without monopoly. This implies the KJV monopoly was MISCLASSIFIED as legitimate coordination (rope) when it was actually a snare with a side benefit. The benefit was real but dispensable. By this logic, the contemporary classification should reweight toward snare and piton, away from rope. However, the engine's mandatrophy resolution requires choosing a single type, and the historical complexity resists closure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_monopoly_necessity,
    'Is a unified translation monopoly structurally necessary for ecclesiastical coordination, or is it contingent on 17th-century political conditions?',
    'Comparative analysis of post-monopoly Protestant churches: do they maintain equivalent doctrinal coordination with multiple authorized translations? Historical counterfactual: what would a federated translation authority have produced?',
    'If necessary: KJV approaches mountain classification (immutable coordination floor). If contingent: constraint is purely tangled_rope (hybrid extraction/coordination that could be decomposed into separate mechanisms).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_monopoly_necessity, conceptual, 'Whether textual monopoly is structurally necessary for ecclesiastical coordination').

omega_variable(
    suppression_mechanism_efficacy,
    'How much of the KJV monopoly''s enforcement came from legal suppression (actual jeopardy to printers/translators) versus cultural prestige and market control?',
    'Historical legal records: frequency of prosecutions for unauthorized translations (1611–1750); economic analysis of printing market concentration; comparison of suppression intensity across regions (England vs. Scotland vs. colonies).',
    'If legal suppression ≥ 60%: constraint is closer to snare. If prestige/market ≥ 60%: constraint is closer to rope with cultural enforcement. Affects suppression metric and χ calculation across all perspectives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(suppression_mechanism_efficacy, empirical, 'Ratio of legal suppression to cultural/market enforcement of KJV monopoly').

omega_variable(
    textual_stability_attribution,
    'What portion of the coordination benefit (liturgical stability, doctrinal coherence, shared reference) flows from the unified translation itself versus from institutional enforcement of a canonical text (which could theoretically apply to any stable text)?',
    'Comparison with Catholic Vulgate monopoly (similar structure, different text); analysis of Protestant communities that maintained doctrinal coherence without translation monopoly (e.g., Reformed traditions using multiple versions); correlation between textual divergence and theological fragmentation in post-1800s pluralist period.',
    'If stability benefit is translation-specific: KJV monopoly is structurally valuable (rope-dominant). If benefit is institutional enforcement-specific: any stable text would provide it (monopoly is pure extraction, snare-dominant). Affects beneficiary analysis and mandatrophy resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_stability_attribution, empirical, 'Attribution of coordination benefits to unified translation versus institutional enforcement').

omega_variable(
    contemporary_piton_inertia,
    'Is the KJV''s contemporary prestige (21st century) due to genuine liturgical/cultural value or pure institutional inertia and nostalgia?',
    'Longitudinal study of KJV usage in Protestant worship (1970–2026): frequency of KJV readings vs alternatives; congregation preference surveys; analysis of new Bible translations'' market share; correlation between KJV prestige and actual linguistic/theological coherence claims vs. perceived authority.',
    'If genuine value: contemporary classification shifts toward rope. If pure inertia: piton classification confirmed. Affects theater_ratio trajectory and mandatrophy resolution in contemporary period.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(contemporary_piton_inertia, empirical, 'Whether contemporary KJV prestige reflects genuine value or institutional inertia').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kjv_textual_authority, 1611, 2026).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kjv_tr_t0, kjv_textual_authority, theater_ratio, 0, 0.48).
narrative_ontology:measurement(kjv_tr_t200, kjv_textual_authority, theater_ratio, 200, 0.6).
narrative_ontology:measurement(kjv_tr_t415, kjv_textual_authority, theater_ratio, 415, 0.64).

% Extraction over time
narrative_ontology:measurement(kjv_be_t0, kjv_textual_authority, base_extractiveness, 0, 0.72).
narrative_ontology:measurement(kjv_be_t200, kjv_textual_authority, base_extractiveness, 200, 0.58).
narrative_ontology:measurement(kjv_be_t415, kjv_textual_authority, base_extractiveness, 415, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kjv_textual_authority, enforcement_mechanism).
narrative_ontology:affects_constraint(kjv_textual_authority, geneva_bible_suppression).
narrative_ontology:affects_constraint(kjv_textual_authority, english_dissenting_tradition).
narrative_ontology:affects_constraint(kjv_textual_authority, protestant_translation_authority).
narrative_ontology:affects_constraint(kjv_textual_authority, biblical_textual_plurality).

% DUAL FORMULATION NOTE:
% The KJV textual monopoly can be decomposed into two structurally distinct constraints: (1) kjv_textual_authority (this story, ε=0.52) — the institutional enforcement and prestige that made KJV dominant; (2) english_translation_coordination (separate story, ε≈0.15) — the genuine coordination problem of choosing a unified translation in a fractured church. The first is tangled_rope (extraction + coordination hybrid). The second is rope (pure coordination). They are linked because the Crown solved the second problem by imposing the first. Network edges show how downstream constraints (dissenting traditions, translation pluralism) are shaped by the upstream monopoly.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kjv_textual_authority, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
