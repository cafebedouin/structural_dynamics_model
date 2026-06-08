% ============================================================================
% CONSTRAINT STORY: native_daily_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_native_daily_reading, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: native_daily_reading
 *   human_readable: Hebrew Native Daily Reading: Vernacular Reconstruction as State-Building Constraint
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The native daily reading of Hebrew as the criterion for linguistic 'life'
 *   represents a specific institutional framing of language revitalization
 *   that emerged from the Zionist state-building project in Palestine/Israel.
 *   This reading asserts that Hebrew ceased to be a 'living language' during
 *   the diaspora period (when it functioned primarily as a liturgical and
 *   literary language) and could only be reconstituted as living through
 *   reconstruction into native vernacular use — daily speech by native
 *   speakers in a territorial context. This reading is one of three competing
 *   framings of the hebrew_living_language kernel: the liturgical reading
 *   (Hebrew remained alive through prayer and study), the continuity
 *   narrative reading (Hebrew maintained unbroken transmission through
 *   diaspora communities), and this native daily reading (Hebrew required
 *   vernacular reconstruction to be truly alive). The native daily reading
 *   carries substantial extractive force because it delegitimizes alternative
 *   framings and imposes costs on communities (particularly Yiddish speakers)
 *   whose linguistic identity does not align with the native daily criterion.
 *   The constraint exhibits genuine coordination function (a shared
 *   vernacular language does enable state formation and institutional
 *   coherence) alongside asymmetric extraction (the benefits flow to those
 *   who adopt Hebrew fastest, the costs to those whose identity is locked to
 *   alternative languages). The measurement trajectory shows extractiveness
 *   rising from 0.35 (1880, early revival period with limited enforcement) to
 *   0.62 (1978, post-statehood with institutional consolidation), while
 *   theater ratio falls from 0.55 to 0.41 (indicating the constraint's
 *   coordination function became more genuine and less performative as
 *   vernacular Hebrew actually became the daily language of a territorial
 *   state).
 *
 * KEY AGENTS:
 *   - Zionist State-Building Project: Primary beneficiary (institutional/arbitrage) — gains linguistic unity, national identity, institutional coherence; can shift policy if needed
 *   - Hebrew Language Authority: Secondary beneficiary (organized/constrained) — gains institutional power through language standardization; constrained by need to maintain legitimacy
 *   - Yiddish-Speaking Diaspora Communities: Primary victim (powerless/identity_locked) — bears costs of cultural discontinuity, intergenerational transmission loss; identity-fused with Yiddish language
 *   - Hebrew-Speaking Immigrants: Mixed position (moderate/constrained) — constrained by language acquisition costs but benefit from cultural capital and social belonging
 *   - Liturgical Hebrew Tradition: Secondary victim (institutional/arbitrage) — loses functional centrality, relegated to performative/ritual status
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing the native daily criterion as linguistic law rather than constructed institutional choice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(native_daily_reading, 0.58).
domain_priors:suppression_score(native_daily_reading, 0.62).
domain_priors:theater_ratio(native_daily_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(native_daily_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(native_daily_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(native_daily_reading, theater_ratio, 0.41).

% --- Constraint claim ---
narrative_ontology:constraint_claim(native_daily_reading, tangled_rope).
narrative_ontology:human_readable(native_daily_reading, "Hebrew Native Daily Reading: Vernacular Reconstruction as State-Building Constraint").
narrative_ontology:topic_domain(native_daily_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(native_daily_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(native_daily_reading, 'b6c9b0d7-5f04-45b1-914d-0eb7f52b704f').
narrative_ontology:cs_kernel_codification('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', formalized).
narrative_ontology:cs_authority_grounding('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', extraction).
narrative_ontology:cs_interpretation_layer_present('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f').
narrative_ontology:cs_reading_relation('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', native_daily_reading__liturgical_reading, forecloses).
narrative_ontology:cs_reading_relation('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', native_daily_reading__continuity_narrative_reading, influences).
narrative_ontology:cs_axiom('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', foundational, vernacular_daily_use_constitutes_linguistic_life).
narrative_ontology:cs_axiom_status(vernacular_daily_use_constitutes_linguistic_life, holdable).
narrative_ontology:cs_axiom_grounding('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', vernacular_daily_use_constitutes_linguistic_life, empirically_contingent).
narrative_ontology:cs_axiom('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', foundational, diaspora_linguistic_identity_is_not_authentic_hebrew).
narrative_ontology:cs_axiom_status(diaspora_linguistic_identity_is_not_authentic_hebrew, holdable).
narrative_ontology:cs_axiom_grounding('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', diaspora_linguistic_identity_is_not_authentic_hebrew, conventional).
narrative_ontology:cs_reference_frame('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', hebrew_living_through_vernacular_reconstruction).
narrative_ontology:cs_drift_state('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', contemporary, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b6c9b0d7-5f04-45b1-914d-0eb7f52b704f', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(native_daily_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(native_daily_reading, zionist_state_building_project).
narrative_ontology:constraint_beneficiary(native_daily_reading, hebrew_institutional_authority).
narrative_ontology:constraint_victim(native_daily_reading, yiddish_cultural_continuity).
narrative_ontology:constraint_victim(native_daily_reading, diaspora_linguistic_identity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(native_daily_reading, hebrew_language_authority).
narrative_ontology:constraint_beneficiary(native_daily_reading, hebrew_speaking_immigrants).
narrative_ontology:constraint_victim(native_daily_reading, yiddish_diaspora_communities).
narrative_ontology:constraint_victim(native_daily_reading, hebrew_speaking_immigrants).
narrative_ontology:constraint_victim(native_daily_reading, liturgical_hebrew_tradition).
narrative_ontology:constraint_vindicates(native_daily_reading, linguistic_nationalism_doctrine).
narrative_ontology:constraint_vindicates(native_daily_reading, territorial_identity_through_language).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the criterion for linguistic life (native daily use) and enforces it through educational policy, institutional prestige allocation, and cultural authority. Can shift policy if needed but benefits from the native daily reading's legitimacy. Gains linguistic unity, national identity, and institutional coherence from the constraint.
narrative_ontology:constraint_stakeholder(native_daily_reading, zionist_state_building_project, agenda_setter,
    institutional, generational, arbitrage, national).

% Administers the native daily criterion through the Hebrew Language Committee, educational curricula, and cultural institutions. Both coordinates genuine linguistic standardization and extracts institutional authority from the monopoly on 'correct' Hebrew. Constrained by the need to maintain legitimacy through linguistic authenticity claims.
narrative_ontology:constraint_stakeholder(native_daily_reading, hebrew_language_authority, agenda_setter,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(native_daily_reading, hebrew_language_authority, beneficiary).

% Bears costs of cultural discontinuity, intergenerational transmission loss, and delegitimization of Yiddish linguistic identity. Identity-fused with Yiddish language; structural mobility exists (could learn Hebrew, could migrate) but identity frame makes exit unthinkable. No mechanism to exit the constraint or to negotiate its terms.
narrative_ontology:constraint_stakeholder(native_daily_reading, yiddish_diaspora_communities, payer,
    powerless, biographical, identity_locked, national).

% Constrained by language acquisition costs and social integration barriers, but benefit from the vernacular shift: Hebrew fluency becomes cultural capital, social belonging, and access to institutional power. Mixed experience: genuine coordination function (shared language enables community formation) alongside asymmetric extraction (those who adopt Hebrew fastest gain advantage).
narrative_ontology:constraint_stakeholder(native_daily_reading, hebrew_speaking_immigrants, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(native_daily_reading, hebrew_speaking_immigrants, payer).

% The pre-existing liturgical Hebrew tradition (prayer, Torah study, rabbinic texts) persists as a parallel system but loses functional centrality to daily life. Maintains cultural authority through ritual performance while being subordinated to the vernacular reading. Retains some arbitrage options (can maintain parallel liturgical practice) but experiences loss of institutional prestige.
narrative_ontology:constraint_stakeholder(native_daily_reading, liturgical_hebrew_tradition, payer,
    institutional, civilizational, arbitrage, global).

% Abstract collective good (not an agent) representing the continuity of diaspora linguistic traditions and intergenerational transmission. Excluded from the native daily reading's framework; would object if represented but is not in the conversation. Bears costs of the constraint without voice or negotiation.
narrative_ontology:constraint_stakeholder(native_daily_reading, diaspora_linguistic_continuity, excluded,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_non_agent(native_daily_reading, diaspora_linguistic_continuity).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unified vernacular language enabling state formation, institutional coherence, and national identity construction. A shared daily language solves the real problem of building a coherent political community from diaspora populations with diverse linguistic backgrounds.
% TRANSFER_FUNCTION: The constraint transfers linguistic prestige, institutional power, and cultural authority from diaspora communities (particularly Yiddish speakers) to the state-building project and Hebrew language authorities. It transfers intergenerational linguistic identity from Yiddish to Hebrew. It transfers institutional resources (educational funding, cultural prestige) toward Hebrew standardization and away from diaspora language maintenance.
% ABSENT_VOICES: Yiddish-speaking diaspora communities would object to the native daily criterion if they were represented in the institutional framework that defines linguistic life. Liturgical Hebrew authorities would object to the subordination of their reading. Diaspora linguistic continuity advocates would object to the delegitimization of alternative linguistic framings. These voices are excluded from the native daily reading's framework because the reading is authored by the state-building project and Hebrew language authorities, not by diaspora communities.
% DISAPPEARANCE_RATIONALE: If the native daily reading disappeared overnight — if the criterion for linguistic life reverted to liturgical use, literary tradition, or diaspora continuity — the institutional arrangements would substantially rearrange. Educational policy would shift (Yiddish and other diaspora languages would regain legitimacy). Cultural prestige allocation would change (liturgical Hebrew would regain institutional authority). The state-building project would lose its linguistic unity mechanism, requiring alternative coordination mechanisms. The constraint's disappearance would not leave the world unchanged; it would restructure institutional authority and cultural legitimacy.
% FOUNDING_PROBLEM: The founding problem was the need to create a unified linguistic basis for a territorial state in Palestine/Israel, drawing on diaspora populations with diverse linguistic backgrounds (Yiddish, Ladino, Arabic, European languages). The Zionist state-building project required a shared language for institutional coherence, national identity, and political community formation. Hebrew was chosen as the language of revival because of its historical and religious significance, and the native daily reading asserts that Hebrew could only serve this function if reconstructed as a living vernacular language.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by historians of the Zionist movement (Gideon Shimoni, David Biale) and sociolinguists (Joshua Fishman, Ghil'ad Zuckermann) who document the state-building necessity for linguistic unity. However, the problem's status as 'dead' is corroborated by the fact that Hebrew is now the daily language of Israel, spoken natively by millions. The founding mandate has been achieved. The constraint's persistence beyond this achievement is maintained by institutional inertia and cultural prestige allocation, not by active state-building necessity. The Hebrew Language Committee continues to enforce the native daily criterion as if the founding mandate were still active, despite its completion.
narrative_ontology:disappearance_verdict(native_daily_reading, world_rearranges).
narrative_ontology:founding_problem_status(native_daily_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: YIDDISH DIASPORA (SNARE) — Identity-locked to Yiddish linguistic and cultural identity; structural mobility exists (could learn Hebrew, could migrate) but identity frame makes exit unthinkable. Vernacular shift imposes costs on cultural continuity, intergenerational transmission, and diaspora cohesion. No exit mechanism; full extraction experienced.
constraint_indexing:constraint_classification(native_daily_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: HEBREW IMMIGRANTS (TANGLED ROPE) — Constrained by language acquisition costs and social integration barriers, but also benefit from the vernacular shift: Hebrew fluency becomes cultural capital, social belonging, and access to institutional power. Mixed experience: genuine coordination function (shared language enables community formation) alongside asymmetric extraction (those who adopt Hebrew fastest gain advantage; those who resist face marginalization).
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: STATE-BUILDING PROJECT (ROPE) — Institutional beneficiary with arbitrage options (can shift language policy, can accommodate multilingualism, can redefine 'native' status). Experiences the constraint as coordination: unified vernacular language enables state formation, institutional coherence, and national identity construction. Net beneficiary — the constraint's extraction flows toward this agent. Theater ratio is low because the coordination function is genuine: a shared daily language does solve the real problem of building a coherent political community from diaspora populations.
constraint_indexing:constraint_classification(native_daily_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: HEBREW LANGUAGE AUTHORITY (TANGLED ROPE) — Organized institutional actors (Hebrew Language Committee, educational authorities, cultural institutions) both coordinate genuine linguistic standardization AND extract authority from the monopoly on 'correct' Hebrew. Constrained by the need to maintain legitimacy through linguistic authenticity claims. Requires active enforcement (curriculum mandates, institutional preference for Hebrew speakers, cultural prestige allocation). Mixed experience: genuine coordination (standardization enables communication) alongside extraction (institutional power concentrated in those who control the language standard).
constraint_indexing:constraint_classification(native_daily_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: LITURGICAL HEBREW TRADITION (PITON) — The pre-existing liturgical Hebrew tradition (prayer, Torah study, rabbinic texts) persists as a parallel system but is increasingly theatrical: it maintains cultural authority through ritual performance while losing functional centrality to daily life. The liturgical reading of Hebrew (as sacred, unchanging, authoritative) coexists with the vernacular reading but is subordinated to it. Theater ratio reflects the shift from functional to performative status of liturgical Hebrew.
constraint_indexing:constraint_classification(native_daily_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, the constraint appears as a natural law: languages require native speakers and daily use to remain 'alive'; without vernacular reconstruction, Hebrew would have remained a liturgical artifact. This perspective naturalizes what is actually a contingent institutional choice (the decision to make Hebrew the state language, the enforcement of vernacular education, the prestige allocation to native speakers). The engine's false summit detector will identify this as naturalization of a constructed constraint.
constraint_indexing:constraint_classification(native_daily_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(native_daily_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(native_daily_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(native_daily_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(native_daily_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(native_daily_reading, TR),
    TR >= 0.70.

:- end_tests(native_daily_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The native daily reading imposes real costs on diaspora communities whose linguistic identity is constituted through Yiddish or other languages. The extraction is not maximal because the coordination function is genuine — a shared vernacular language does solve real problems of state formation and institutional coherence. The beneficiary (state-building project) captures substantial benefits, but these are not purely extractive; they represent legitimate coordination gains. The measurement trajectory shows extractiveness rising over time as the constraint's enforcement intensified (from early revival movement to post-statehood institutional consolidation). Suppression (0.62): Moderate-high. Significant barriers to exit include: identity-lock (Yiddish identity makes Hebrew adoption unthinkable from within the frame), institutional pressure (educational mandates, cultural prestige allocation to Hebrew speakers), and economic costs (language acquisition requires time and resources). Suppression is not total because structural mobility exists — diaspora communities could learn Hebrew, could migrate, could adopt the native daily reading — but the identity-lock mechanism makes these options experientially unavailable. Theater ratio (0.41): Moderate-low. The constraint's coordination function is genuine enough that theater is not dominant. Unlike pure extraction mechanisms that require performative legitimation, the native daily reading's legitimacy rests partly on the actual fact that Hebrew did become a daily language in Palestine/Israel. However, theater persists in the framing itself: the claim that Hebrew 'required' vernacular reconstruction to be 'alive' is a constructed criterion that naturalizes a political choice.
 *
 * PERSPECTIVAL GAP:
 *   The native daily reading exhibits a sharp perspectival gap between beneficiaries and victims. The state-building project sees coordination (Rope) — a shared language enabling political community. The Hebrew language authority sees mixed coordination and institutional power (Tangled Rope) — genuine standardization alongside authority concentration. The Yiddish diaspora sees pure extraction (Snare) — costs imposed with no exit mechanism and no benefit. The Hebrew immigrants see mixed experience (Tangled Rope) — constrained by acquisition costs but benefiting from cultural capital. The liturgical tradition sees degradation (Piton) — its previous functional role subordinated to performative status. The analytical observer risks seeing natural law (Mountain) — that languages require native speakers to be 'alive' — but the structural data reveals this as a false summit: the native daily criterion is a constructed institutional choice that serves the state-building project.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality (d) is determined by the agent's structural position relative to the extraction flow. The state-building project (institutional/arbitrage) experiences low d because it is the beneficiary and has exit options (can shift policy). The Yiddish diaspora (powerless/identity_locked) experiences high d because it is the victim and has no exit mechanism — the identity-lock prevents exercising structural mobility. The Hebrew immigrants (moderate/constrained) experience moderate d because they are partly constrained (acquisition costs) but also partly benefiting (cultural capital). The Hebrew language authority (organized/constrained) experiences moderate d because it both coordinates (genuine standardization) and extracts (institutional power concentration). The liturgical tradition (institutional/arbitrage) experiences moderate d because it is a victim of the reading's dominance but retains some arbitrage options (can maintain parallel liturgical practice). The analytical observer (analytical/analytical) experiences no d — the analytical position is outside the extraction flow, but risks being captured by the false summit framing.
 *
 * MANDATROPHY ANALYSIS:
 *   The native daily reading exhibits potential mandatrophy: the founding mandate was to revitalize Hebrew as a living language for state-building purposes. This mandate has been achieved — Hebrew is now the daily language of Israel, spoken natively by millions. However, the constraint persists beyond its founding purpose: the native daily criterion continues to delegitimize alternative linguistic framings (liturgical, Yiddish, diaspora continuity) even though the state-building mandate is complete. The constraint's persistence is now maintained through institutional inertia and cultural prestige allocation rather than through active state-building necessity. The measurement trajectory shows suppression remaining high (0.62) even as the founding mandate is satisfied, suggesting the constraint has become self-perpetuating. Mandatrophy is not yet resolved because the institutional authority (Hebrew Language Committee, educational system) continues to enforce the native daily criterion as if the founding mandate were still active.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_choice,
    'Is the requirement for native daily use to constitute linguistic ''life'' a natural law of linguistics, or a constructed criterion that serves the state-building project?',
    'Comparative analysis of language revitalization movements: do all successful revivals require native daily use, or do some maintain linguistic vitality through other mechanisms (liturgical use, literary tradition, institutional standardization without vernacularization)? Historical examination of whether the ''native daily use'' criterion was discovered or invented by the Hebrew revival movement.',
    'If natural law: the constraint is mountain-type, beneficiaries are incidental, and the extraction is justified as necessary cost. If constructed: the constraint is tangled_rope or snare, beneficiaries are primary, and the extraction is a choice with alternatives.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_choice, conceptual, 'Whether native daily use requirement is natural linguistic law or constructed criterion').

omega_variable(
    yiddish_continuity_counterfactual,
    'Could Yiddish have been revitalized as the state language instead of Hebrew, and if so, what structural differences would the constraint exhibit?',
    'Historical analysis of the Yiddish revival movement (YIVO, Yiddish literature, secular Yiddish culture); examination of why Hebrew was chosen over Yiddish despite Yiddish''s greater existing speaker base; counterfactual modeling of state formation with Yiddish as the official language.',
    'If Yiddish revitalization was structurally possible: the Hebrew choice was contingent, and the constraint''s beneficiary structure is revealed as political choice rather than linguistic necessity. If Yiddish revitalization was structurally impossible: the Hebrew choice was overdetermined, and the constraint''s extraction is more justified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(yiddish_continuity_counterfactual, empirical, 'Whether Yiddish revitalization was a viable alternative to Hebrew').

omega_variable(
    reading_committer_ambiguity,
    'This constraint instantiates the ''native daily reading'' of the hebrew_living_language kernel. Does this reading foreclose the liturgical_reading (sacred, unchanging Hebrew), or do they coexist as parallel legitimate framings?',
    'Examination of institutional practice: do Hebrew language authorities treat liturgical and vernacular Hebrew as competing or complementary? Do speakers experience them as mutually exclusive identity commitments, or as layered uses of the same language? Historical analysis of whether the vernacular shift required explicit rejection of the liturgical reading or merely subordination of it.',
    'If forecloses: the native daily reading is the sole legitimate reading within the state framework, and the liturgical reading is relegated to private/religious sphere. If coexists: both readings remain live, and the constraint exhibits lower suppression (multiple legitimate framings). If influences: the vernacular reading creates structural pressure on the liturgical reading without eliminating it.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_ambiguity, conceptual, 'Relationship between native daily reading and liturgical reading of Hebrew').

omega_variable(
    diaspora_linguistic_identity_lock,
    'Is the Yiddish-speaking diaspora''s resistance to Hebrew vernacularization a structural barrier (economic cost of language acquisition, institutional exclusion) or an identity-locked cognitive frame (Yiddish identity constituted through the language, making exit unthinkable)?',
    'Longitudinal study of diaspora communities: do second-generation immigrants adopt Hebrew when structural barriers are removed (economic opportunity, institutional access)? Do they maintain Yiddish identity despite fluency in Hebrew? Analysis of whether Yiddish identity persists as a choice or as an internalized frame that persists even when structural barriers disappear.',
    'If structural: suppression is high but not total; exit is possible at a cost. If identity-locked: suppression is lower structurally but higher experientially; the agent carries the lock with them even after exit. If both: the constraint exhibits both structural and internalized suppression mechanisms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(diaspora_linguistic_identity_lock, empirical, 'Whether Yiddish diaspora resistance is structural or identity-locked').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(native_daily_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ndr_theater_1880, native_daily_reading, theater_ratio, 0, 0.55).
narrative_ontology:measurement(ndr_theater_1920, native_daily_reading, theater_ratio, 10, 0.48).
narrative_ontology:measurement(ndr_theater_1948, native_daily_reading, theater_ratio, 20, 0.38).
narrative_ontology:measurement(ndr_theater_1978, native_daily_reading, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(ndr_extractiveness_1880, native_daily_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(ndr_extractiveness_1920, native_daily_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(ndr_extractiveness_1948, native_daily_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(ndr_extractiveness_1978, native_daily_reading, base_extractiveness, 30, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(ndr_suppression_1880, native_daily_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(ndr_suppression_1920, native_daily_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(ndr_suppression_1948, native_daily_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(ndr_suppression_1978, native_daily_reading, suppression_requirement, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(native_daily_reading, identity_coordination).
narrative_ontology:affects_constraint(native_daily_reading, yiddish_language_suppression).
narrative_ontology:affects_constraint(native_daily_reading, diaspora_identity_fragmentation).
narrative_ontology:affects_constraint(native_daily_reading, hebrew_institutional_authority).

% DUAL FORMULATION NOTE:
% The native daily reading is one of three structurally distinct constraints within the hebrew_living_language kernel family. The liturgical_reading has different ε (lower extractiveness, lower suppression, higher theater) because it does not impose costs on diaspora communities and does not require enforcement. The continuity_narrative_reading has different ε (lower extractiveness, lower suppression) because it legitimizes diaspora linguistic identity rather than delegitimizing it. Each reading should be authored as a separate constraint story with its own perspectives, beneficiary/victim structure, and measurements. The three stories are linked through network.affects_constraints to show their structural interdependence within the kernel family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(native_daily_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
