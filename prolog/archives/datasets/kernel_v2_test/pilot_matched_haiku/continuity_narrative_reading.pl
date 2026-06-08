% ============================================================================
% CONSTRAINT STORY: continuity_narrative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_narrative_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: continuity_narrative_reading
 *   human_readable: Continuity Narrative Reading of Hebrew Language Revival
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The continuity narrative reading of Hebrew language revival frames the
 *   19th-20th century revival of Hebrew as a restoration of an unbroken
 *   Jewish linguistic tradition, rather than as a constructed modern language
 *   project. This reading instantiates one interpretation of the contested
 *   kernel 'hebrew_living_language' — the claim that Hebrew is the authentic,
 *   natural, continuous Jewish language. The narrative asserts that Hebrew
 *   never truly died, that its revival was a return to a natural state, and
 *   that linguistic continuity is inherent to Jewish identity. This reading
 *   is one of three structurally distinct constraints within the same kernel:
 *   the liturgical_reading (Hebrew as sacred language maintained through
 *   religious practice), the native_daily_reading (Hebrew as a modern
 *   constructed language deliberately created for daily use), and this
 *   continuity_narrative_reading (Hebrew as naturally continuous, never truly
 *   interrupted). Each reading has different extractiveness values, different
 *   beneficiaries and victims, and different institutional enforcement
 *   mechanisms. The continuity narrative reading exhibits moderate-to-high
 *   extractiveness (0.58) because it subordinates historical accuracy to
 *   legitimacy claims, suppresses counter-narratives, and extracts
 *   nationalist legitimacy from the claim of natural restoration. The
 *   narrative's theater ratio (0.68) reflects that much of its enforcement is
 *   performative — the narrative persists through institutional consensus and
 *   authority-grounding in founding Zionist scholars rather than through
 *   ongoing empirical vindication.
 *
 * KEY AGENTS:
 *   - Israeli State and Zionist Institutional Complex: Primary beneficiary (institutional/arbitrage) — gains nationalist legitimacy from restoration narrative; can shift narratives if needed
 *   - Hebrew Language Establishment: Primary beneficiary (institutional/constrained) — gains institutional support, funding, prestige, curriculum authority; bears costs of enforcing the narrative
 *   - Counter-Narrative Historians: Primary victim (powerless/trapped) — face professional suppression, citation exclusion, institutional barriers; cannot exit without abandoning research program
 *   - Yiddish Diaspora Linguistic Heritage: Primary victim (powerless/identity_locked) — subordinated to supporting role in restoration story; identity-locked because narrative reframes heritage as inauthentic
 *   - Hebrew Language Educators and Linguists: Secondary agent (moderate/constrained) — benefit from narrative but bear costs of enforcement; constrained exit due to professional standing
 *   - Diaspora Jewish Communities: Organized agent (organized/constrained) — benefit from unifying linguistic identity; bear costs of suppressing Yiddish heritage and managing cognitive dissonance
 *   - Academic Consensus Enforcement Apparatus: Institutional actor (institutional/arbitrage) — maintains narrative through institutional inertia; largely performative (piton perspective)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_narrative_reading, 0.58).
domain_priors:suppression_score(continuity_narrative_reading, 0.62).
domain_priors:theater_ratio(continuity_narrative_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_narrative_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(continuity_narrative_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(continuity_narrative_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_narrative_reading, tangled_rope).
narrative_ontology:human_readable(continuity_narrative_reading, "Continuity Narrative Reading of Hebrew Language Revival").
narrative_ontology:topic_domain(continuity_narrative_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(continuity_narrative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_narrative_reading, '8a13bab1-8024-477a-ba08-44ceb71d544c').
narrative_ontology:cs_kernel_codification('8a13bab1-8024-477a-ba08-44ceb71d544c', formalized).
narrative_ontology:cs_authority_grounding('8a13bab1-8024-477a-ba08-44ceb71d544c', extraction).
narrative_ontology:cs_interpretation_layer_present('8a13bab1-8024-477a-ba08-44ceb71d544c').
narrative_ontology:cs_reading_relation('8a13bab1-8024-477a-ba08-44ceb71d544c', continuity_narrative_reading__hebrew_liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('8a13bab1-8024-477a-ba08-44ceb71d544c', continuity_narrative_reading__hebrew_native_daily_reading, influences).
narrative_ontology:cs_axiom('8a13bab1-8024-477a-ba08-44ceb71d544c', foundational, hebrew_linguistic_continuity_unbroken).
narrative_ontology:cs_axiom_status(hebrew_linguistic_continuity_unbroken, holdable).
narrative_ontology:cs_axiom_grounding('8a13bab1-8024-477a-ba08-44ceb71d544c', hebrew_linguistic_continuity_unbroken, empirically_contingent).
narrative_ontology:cs_axiom('8a13bab1-8024-477a-ba08-44ceb71d544c', foundational, revival_as_natural_restoration_not_construction).
narrative_ontology:cs_axiom_status(revival_as_natural_restoration_not_construction, holdable).
narrative_ontology:cs_axiom_grounding('8a13bab1-8024-477a-ba08-44ceb71d544c', revival_as_natural_restoration_not_construction, empirically_contingent).
narrative_ontology:cs_reference_frame('8a13bab1-8024-477a-ba08-44ceb71d544c', hebrew_natural_continuity_unbroken).
narrative_ontology:cs_drift_state('8a13bab1-8024-477a-ba08-44ceb71d544c', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8a13bab1-8024-477a-ba08-44ceb71d544c', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(continuity_narrative_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, israeli_nationalist_legitimacy_project).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, hebrew_language_establishment).
narrative_ontology:constraint_victim(continuity_narrative_reading, counter_narrative_historical_accuracy).
narrative_ontology:constraint_victim(continuity_narrative_reading, yiddish_diaspora_linguistic_heritage).
narrative_ontology:constraint_victim(continuity_narrative_reading, constructed_language_acknowledgment).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, hebrew_educators_linguists).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, diaspora_jewish_communities).
narrative_ontology:constraint_victim(continuity_narrative_reading, counter_narrative_historians).
narrative_ontology:constraint_victim(continuity_narrative_reading, yiddish_diaspora_heritage).
narrative_ontology:constraint_victim(continuity_narrative_reading, hebrew_educators_linguists).
narrative_ontology:constraint_vindicates(continuity_narrative_reading, unbroken_jewish_linguistic_continuity).
narrative_ontology:constraint_vindicates(continuity_narrative_reading, hebrew_as_natural_restoration).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the official narrative of Hebrew revival as restoration of natural Jewish language. Controls educational curricula, media representation, and institutional authority. Benefits from the narrative's legitimacy for the state project. Can shift narratives if needed without losing institutional power.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, israeli_state_zionist_complex, agenda_setter,
    institutional, immediate, arbitrage, national).

% Gains institutional support, funding, prestige, and curriculum authority from the continuity narrative. Enforces the narrative through textbook standards, peer review gatekeeping, and conference norms. Bears costs of defending implausible claims and suppressing counter-evidence. Cannot exit without losing professional standing.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, hebrew_language_establishment, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(continuity_narrative_reading, hebrew_language_establishment, agenda_setter).

% Scholars documenting Hebrew as constructed/revived language face professional suppression, citation exclusion, and institutional barriers. Their research contradicts the restoration narrative and is treated as delegitimizing the entire revival project. Cannot exit the field without abandoning their research program and career investment.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, counter_narrative_historians, payer,
    powerless, biographical, trapped, global).

% Yiddish speakers and their descendants are structurally locked into a narrative that treats their linguistic identity as a temporary deviation from 'true' Jewish language. The narrative reframes Yiddish as the 'exile language' and Hebrew as the 'return.' Exit would require abandoning the claim that Yiddish was ever legitimately Jewish. Suppression is internalized through the narrative's authority.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, yiddish_diaspora_heritage, payer,
    powerless, generational, identity_locked, global).

% Benefit from institutional support, funding, and prestige while bearing costs of enforcing the continuity narrative. Must defend implausible claims about unbroken linguistic continuity and suppress counter-evidence. Constrained exit due to professional standing and institutional position.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, hebrew_educators_linguists, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(continuity_narrative_reading, hebrew_educators_linguists, payer).

% Organized agents (Jewish organizations, cultural institutions, educational bodies) benefit from the continuity narrative as a unifying linguistic identity connecting diaspora to Israel. Bear costs of enforcing the narrative through suppressing Yiddish heritage and managing cognitive dissonance. Constrained exit due to institutional standing and community cohesion.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, diaspora_jewish_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(continuity_narrative_reading, diaspora_jewish_communities, agenda_setter).

% Institutional machinery (textbook standards, peer review gatekeeping, citation networks, conference norms) that maintains the continuity narrative. Enforces the narrative through institutional inertia and authority-grounding in founding Zionist scholars rather than through ongoing empirical vindication. Largely performative — maintains consensus through ritual rather than active research.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, academic_consensus_apparatus, agenda_setter,
    institutional, civilizational, arbitrage, global).

% The principle that historical claims should be evaluated on empirical evidence is subordinated to the legitimacy claims of the continuity narrative. Historical accuracy is treated as secondary to nationalist legitimacy. This is a non-agent entity (a principle, not an actor) but is included to show what the narrative extracts from.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, historical_accuracy_principle, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(continuity_narrative_reading, historical_accuracy_principle).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Unifying diaspora Jews around a shared linguistic identity for return to Israel; creating a common language for Jewish state-building; maintaining Jewish cultural continuity across diaspora and homeland.
% TRANSFER_FUNCTION: The narrative transfers legitimacy and nationalist authority from the state to the claim of natural Hebrew continuity. It transfers suppression and identity subordination from the state to counter-narrative researchers and Yiddish speakers. It transfers institutional prestige and funding from the state to Hebrew language establishment.
% ABSENT_VOICES: Yiddish speakers and their descendants are partially excluded from the conversation — their linguistic heritage is subordinated to the restoration narrative. Counter-narrative historians are excluded from institutional authority and peer review gatekeeping. Secular Hebrew speakers who view the language as modern construction rather than restoration are excluded from official discourse.
% DISAPPEARANCE_RATIONALE: If the continuity narrative disappeared, Israeli national identity would require reconstruction. The state's legitimacy claim (return to natural Jewish homeland) would be undermined. Educational curricula would need revision. Diaspora-Israel linguistic connection would be reframed. Yiddish heritage would be rehabilitated. Counter-narrative research would be institutionally legitimized. The narrative's disappearance would rearrange the entire institutional and identity landscape of Hebrew language and Jewish nationalism.
% FOUNDING_PROBLEM: In the late 19th century, Hebrew was a liturgical language with no native speakers. The founding problem was: how can a Jewish state be built if there is no shared living language among diaspora Jews? The continuity narrative solved this by asserting that Hebrew was never truly dead, that its revival was restoration rather than construction, and that linguistic continuity was inherent to Jewish identity. This framing made the revival seem natural and inevitable rather than artificial and contested.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (lack of shared living language) was real and is attested by historical records of the early Zionist movement. However, the problem is now dead — Hebrew is a living language with millions of native speakers. Modern Hebrew speakers do not need the continuity narrative to justify their language; the language's existence is self-justifying. Yet the narrative persists, suggesting it has become an extraction mechanism rather than a coordination solution. Corroboration comes from linguists (Shlomo Izre'el, Benjamin Harshav) who document the constructed nature of modern Hebrew, and from the historical record of deliberate language planning (Ben-Yehuda's neologisms, grammar standardization, vocabulary creation).
narrative_ontology:disappearance_verdict(continuity_narrative_reading, world_rearranges).
narrative_ontology:founding_problem_status(continuity_narrative_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COUNTER-NARRATIVE HISTORIANS (SNARE) — Scholars documenting Hebrew as a constructed/revived language face professional suppression, citation exclusion, and institutional barriers. Cannot exit the field without abandoning their research program. The continuity narrative's enforcement mechanism directly targets this agent: historical accuracy that contradicts the restoration myth is treated as delegitimizing the entire revival project. Maximum extraction — trapped by career investment in the field, suppressed by institutional gatekeeping.
constraint_indexing:constraint_classification(continuity_narrative_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: YIDDISH DIASPORA LINGUISTIC HERITAGE (SNARE) — The continuity narrative subordinates Yiddish (the actual living Jewish language for centuries) to a supporting role in the Hebrew restoration story. Yiddish speakers and their descendants are structurally locked into a narrative that treats their linguistic identity as a temporary deviation from 'true' Jewish language. Identity-locked because the narrative reframes their heritage as inauthentic — exit would require abandoning the claim that their language was ever legitimately Jewish. Suppression is internalized through the narrative's authority: Yiddish becomes the 'exile language,' Hebrew the 'return.' High extraction, high suppression.
constraint_indexing:constraint_classification(continuity_narrative_reading, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(identity_locked),
            spatial_scope(global))).

% PERSPECTIVE 3: HEBREW LANGUAGE EDUCATORS AND LINGUISTS (TANGLED ROPE) — Benefit from the continuity narrative (institutional support, funding, prestige, curriculum authority) while also bearing costs of enforcing it (suppressing counter-evidence, managing cognitive dissonance, defending implausible claims). Constrained exit: abandoning the narrative risks professional standing and institutional position. The constraint coordinates genuine language pedagogy while extracting conformity to the restoration myth. Moderate extraction because the beneficiary role is real but the enforcement burden is substantial.
constraint_indexing:constraint_classification(continuity_narrative_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ISRAELI STATE AND ZIONIST INSTITUTIONAL COMPLEX (ROPE) — Primary beneficiary. The continuity narrative provides legitimacy for the state project: Hebrew revival as restoration (not colonization) of a natural Jewish linguistic homeland. The narrative solves a genuine coordination problem (unifying diaspora Jews around a shared language) while simultaneously extracting nationalist legitimacy. Arbitrage exit: the state can shift narratives if needed (e.g., emphasizing pragmatic modernization over continuity) without losing institutional power. Net beneficiary — extraction flows toward this agent.
constraint_indexing:constraint_classification(continuity_narrative_reading, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ACADEMIC CONSENSUS ENFORCEMENT APPARATUS (PITON) — The institutional machinery that maintains the continuity narrative (textbook standards, peer review gatekeeping, citation networks, conference norms) is largely performative. The narrative persists through institutional inertia and authority-grounding in lineage (the founding Zionist scholars) rather than through ongoing empirical vindication. Reviewers and editors enforce the narrative not because it is empirically strongest but because it is institutionally established. Theater ratio high (0.68) — much of the enforcement is ritual maintenance of consensus rather than active research. Piton classification: degraded function (the narrative no longer needs defending against serious counter-evidence because counter-evidence is suppressed) maintained as performance (the apparatus continues as if the narrative were contested).
constraint_indexing:constraint_classification(continuity_narrative_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: DIASPORA JEWISH COMMUNITIES (TANGLED ROPE) — Organized agents (Jewish organizations, cultural institutions, educational bodies) benefit from the continuity narrative (it provides a unifying linguistic identity, connects diaspora to Israel, offers cultural legitimacy) while bearing costs of enforcing it (suppressing Yiddish heritage, managing the cognitive dissonance of 'return' narratives, constraining linguistic diversity). Constrained exit: abandoning the narrative risks institutional standing and community cohesion. The constraint coordinates diaspora-Israel linguistic connection while extracting conformity to the restoration myth.
constraint_indexing:constraint_classification(continuity_narrative_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the continuity narrative appears as an immutable feature of Jewish identity: the claim that Hebrew is the 'natural' Jewish language, that its revival is restoration rather than construction, that linguistic continuity is inherent to Jewish peoplehood. This perspective naturalizes the narrative as a law of Jewish cultural identity. However, the structural data contradicts this classification — the engine will compute this as a false summit, revealing that the 'natural continuity' framing is a constructed narrative with identifiable beneficiaries and victims.
constraint_indexing:constraint_classification(continuity_narrative_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_narrative_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(continuity_narrative_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(continuity_narrative_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(continuity_narrative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(continuity_narrative_reading, TR),
    TR >= 0.70.

:- end_tests(continuity_narrative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The continuity narrative extracts nationalist legitimacy and institutional authority by subordinating historical accuracy. The measurement trajectory shows rising extractiveness over the interval (0.35 → 0.62), reflecting how the narrative's enforcement has intensified as counter-evidence has accumulated. Early in the revival (t=0), the narrative was plausible and required less enforcement; as linguistic construction became undeniable, suppression of counter-narratives increased. Suppression (0.62): Moderate-high. Significant barriers to counter-narrative research include institutional gatekeeping, citation exclusion, career risk, and the narrative's embedding in educational curricula and national identity. Suppression has risen over the interval (0.25 → 0.62) as the narrative became institutionalized. Theater ratio (0.68): High. Much of the narrative's enforcement is performative — the apparatus maintains consensus through ritual (textbook standards, peer review gatekeeping, conference norms) rather than through active research. The theater ratio has risen (0.40 → 0.68) as the narrative has shifted from active construction to institutional maintenance. Claimed type (tangled_rope): The narrative coordinates genuine language pedagogy and diaspora-Israel connection while extracting conformity to the restoration myth. Active enforcement is required to suppress counter-narratives and maintain the narrative's authority.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how the same structural phenomenon — the revival of Hebrew as a living language — classifies differently depending on the observer's position. The Israeli state sees coordination (Rope) — solving the problem of unifying diaspora Jews around a shared language. Hebrew educators see mixed coordination and extraction (Tangled Rope) — the narrative enables pedagogy but requires enforcing implausible claims. Counter-narrative historians see pure extraction (Snare) — their research is suppressed to maintain the narrative's authority. Yiddish speakers see identity-locked extraction (Snare) — their heritage is subordinated and reframed as inauthentic. The academic apparatus sees its own degraded ritual (Piton) — the narrative persists through institutional inertia rather than empirical vindication. The civilizational analytical observer risks seeing an immutable natural law (Mountain) — linguistic continuity as inherent to Jewish identity — but the structural data reveals this as a false summit: the narrative is a constructed legitimacy claim with identifiable beneficiaries and victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value (d) is determined by the agent's structural position relative to the narrative's extraction flow. The Israeli state and Hebrew establishment are beneficiaries with arbitrage options (low d, negative effective extraction). Counter-narrative historians are victims with trapped exit (high d, high effective extraction). Yiddish speakers are victims with identity-locked exit (high d, high effective extraction, with internalized suppression). Hebrew educators are beneficiaries with constrained exit (moderate d, moderate effective extraction). Diaspora communities are beneficiaries with constrained exit (moderate d, moderate effective extraction). The academic apparatus is a beneficiary with arbitrage options (low d, negative effective extraction). The directionality derivation reflects that the narrative's extraction is asymmetric: it flows toward the state and institutional actors, away from counter-narrative researchers and Yiddish speakers.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity narrative reading exhibits mandatrophy: its original mandate was to solve a genuine coordination problem (unifying diaspora Jews around a shared language for return to Israel), but the narrative has outlived its functional necessity. Modern Hebrew is now a living language with millions of native speakers — the revival is complete. Yet the narrative persists, not because it solves the original coordination problem, but because it provides nationalist legitimacy and institutional authority. The narrative's mandate (establishing Hebrew as the natural Jewish language) has been achieved, but the narrative itself has become an extraction mechanism that suppresses counter-narratives and subordinates Yiddish heritage. The mandatrophy is not resolved — the narrative continues to extract legitimacy and enforce conformity despite its original function being obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_ambiguity,
    'Is the continuity narrative a description of actual linguistic continuity, or a constructed legitimacy claim that naturalizes a deliberate revival project?',
    'Historical linguistic analysis comparing Hebrew''s actual usage patterns (liturgical only, not daily speech) before 1880 with the narrative''s claims of unbroken continuity. Comparison with documented construction efforts (Ben-Yehuda''s neologisms, grammar standardization, vocabulary creation). Analysis of how similar language revivals (Irish, Icelandic, Welsh) are framed differently.',
    'If actual continuity: mountain classification confirmed, beneficiaries are incidental. If constructed: tangled_rope confirmed, the narrative is an extraction mechanism disguised as restoration. This is the core ambiguity the reading instantiates.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_vs_constructed_ambiguity, empirical, 'Whether continuity narrative describes actual linguistic continuity or constructed revival').

omega_variable(
    suppression_mechanism_internalization,
    'Is the suppression of counter-narratives structural (institutional gatekeeping, career barriers) or internalized (the narrative has become identity-constitutive for Hebrew speakers and Israeli Jews)?',
    'Post-institutional analysis: if institutional suppression were removed (e.g., academic freedom protections for counter-narrative research), would counter-narratives persist? Interviews with scholars who have attempted to publish counter-narratives. Analysis of how the narrative is transmitted in educational contexts (is it presented as settled fact or as one interpretation among others?).',
    'If structural: suppression can be reduced through institutional reform. If internalized: the narrative is identity-locked for many agents, and exit requires identity reconstruction. This affects the exit_options classification for multiple perspectives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression is structural or internalized in the continuity narrative').

omega_variable(
    yiddish_heritage_victim_status,
    'Is Yiddish linguistic heritage a victim of the continuity narrative, or a complementary tradition that coexists with Hebrew revival?',
    'Historical analysis of institutional support for Yiddish vs Hebrew in Israel and diaspora (funding, curriculum time, prestige). Analysis of how Yiddish is framed in Hebrew-dominant contexts (as ''exile language,'' ''diaspora deviation,'' or as legitimate heritage). Comparison with multilingual contexts where multiple languages are treated as equally authentic.',
    'If victim: the narrative extracts legitimacy from Yiddish speakers by subordinating their heritage. If coexistent: the narrative is coordination without extraction. This determines whether Yiddish speakers should be classified as victims.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(yiddish_heritage_victim_status, empirical, 'Whether Yiddish heritage is victim of continuity narrative or coexistent tradition').

omega_variable(
    reading_vs_sibling_foreclosure,
    'Does the continuity narrative reading logically foreclose the native_daily_reading (Hebrew as modern constructed language) within a single institutional framework, or do they coexist as competing interpretations?',
    'Analysis of whether Israeli institutions (schools, media, government) present both readings as live options or enforce one as canonical. Examination of whether scholars can hold both readings simultaneously without institutional penalty. Historical analysis of how the readings have competed for authority.',
    'If foreclosure: the readings are mutually exclusive within the commitment system. If coexistence: they are competing interpretations that different parties hold. This determines the reading_relations classification in cs_structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_vs_sibling_foreclosure, conceptual, 'Whether continuity reading forecloses or coexists with native_daily reading').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_narrative_reading, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_narr_tr_t0, continuity_narrative_reading, theater_ratio, 0, 0.4).
narrative_ontology:measurement(cont_narr_tr_t20, continuity_narrative_reading, theater_ratio, 20, 0.55).
narrative_ontology:measurement(cont_narr_tr_t40, continuity_narrative_reading, theater_ratio, 40, 0.68).
narrative_ontology:measurement(cont_narr_tr_t60, continuity_narrative_reading, theater_ratio, 60, 0.68).

% Extraction over time
narrative_ontology:measurement(cont_narr_be_t0, continuity_narrative_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cont_narr_be_t20, continuity_narrative_reading, base_extractiveness, 20, 0.48).
narrative_ontology:measurement(cont_narr_be_t40, continuity_narrative_reading, base_extractiveness, 40, 0.58).
narrative_ontology:measurement(cont_narr_be_t60, continuity_narrative_reading, base_extractiveness, 60, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(cont_narr_su_t0, continuity_narrative_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cont_narr_su_t20, continuity_narrative_reading, suppression_requirement, 20, 0.45).
narrative_ontology:measurement(cont_narr_su_t40, continuity_narrative_reading, suppression_requirement, 40, 0.62).
narrative_ontology:measurement(cont_narr_su_t60, continuity_narrative_reading, suppression_requirement, 60, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_narrative_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(continuity_narrative_reading, 0.12).
narrative_ontology:affects_constraint(continuity_narrative_reading, hebrew_liturgical_reading).
narrative_ontology:affects_constraint(continuity_narrative_reading, hebrew_native_daily_reading).
narrative_ontology:affects_constraint(continuity_narrative_reading, yiddish_linguistic_subordination).
narrative_ontology:affects_constraint(continuity_narrative_reading, israeli_nationalist_legitimacy).

% DUAL FORMULATION NOTE:
% The continuity narrative reading is one of three structurally distinct constraints within the hebrew_living_language kernel. Each reading has different extractiveness values, different beneficiaries and victims, and different institutional enforcement mechanisms. The continuity reading (this story) has moderate-high extractiveness (0.58) because it subordinates historical accuracy to legitimacy claims. The liturgical reading has lower extractiveness (coordination of religious practice). The native_daily reading has higher extractiveness (construction of a modern language against resistance). All three readings are linked via network.affects_constraints to show their structural interdependence within the kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(continuity_narrative_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
