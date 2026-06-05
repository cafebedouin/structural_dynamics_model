% ============================================================================
% CONSTRAINT STORY: hebrew_living_language__liturgical_preservation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language__liturgical_preservation_reading, []).

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
 *   constraint_id: hebrew_living_language__liturgical_preservation_reading
 *   human_readable: Hebrew Living Language: Liturgical Preservation Reading
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates ONE READING of the contested kernel
 *   'Hebrew lives.' Under the liturgical preservation reading, Hebrew
 *   continuity is secured through sacred textual preservation and ritualized
 *   recitation across generations, without requiring native intergenerational
 *   speakers. This reading treats Hebrew as a sacred language whose primary
 *   function is religious liturgy — prayer, scriptural study, halakhic
 *   interpretation. The constraint operates by: (1) institutional mechanisms
 *   that transmit canonical texts (scribal traditions, print standardization,
 *   educational transmission), (2) social mechanisms that sanctify the
 *   language itself (linguistic purity as religious value, 'profane' uses as
 *   transgression), (3) authority structures that adjudicate correct forms
 *   (rabbinical schools, textual scholarship). The expected structural delta
 *   distinguishes this reading from siblings: the victim set includes
 *   secularizers who wish to use Hebrew outside the sacred frame (treating
 *   such use as 'profaning' the sacred tongue); extractiveness is low (only
 *   ritual maintenance and textual preservation costs, no asymmetric
 *   extraction from a dominated class); no native speakers are required — the
 *   constraint defines Hebrew as living through symbolic preservation, not
 *   through daily vernacular use. This reading has been institutionally
 *   dominant in diaspora Jewish communities (medieval through early modern
 *   period) and remains influential in traditional religious contexts.
 *
 * KEY AGENTS:
 *   - Liturgical Community (Organized): Prayer communities, synagogues, yeshivas — organized practitioners solving genuine coordination problem of shared ritual language. Net beneficiary through enabling synchronized prayer.
 *   - Textual Authority (Institutional): Rabbinical academies, scribal traditions, print authority structures — institutional preservers maintaining canonical forms. Benefit from prestige and authority to adjudicate correct usage.
 *   - Secularizing Speaker (Powerless/Trapped): Jews seeking to use Hebrew as modern vernacular while respecting its historical sacredness. Experience suppression through religious authority resistance to 'profane' uses and denial that secular Hebrew is legitimate.
 *   - Bilingual Speaker (Moderate/Constrained): Fluent in both liturgical and modern Hebrew — constrained participant experiencing both coordination benefits and friction costs. Can exit but benefits exceed costs.
 *   - Academic Hebraist (Institutional): Modern university scholars studying biblical/rabbinic Hebrew — reproduce sacred preservation protocols performatively without the functional liturgical need that justifies them in religious contexts.
 *   - Analytical Observer (Civilizational): Structural perspective recognizing this reading as one legitimate frame among competing kernel readings.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language__liturgical_preservation_reading, 0.18).
domain_priors:suppression_score(hebrew_living_language__liturgical_preservation_reading, 0.32).
domain_priors:theater_ratio(hebrew_living_language__liturgical_preservation_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_preservation_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_preservation_reading, suppression_requirement, 0.32).
narrative_ontology:constraint_metric(hebrew_living_language__liturgical_preservation_reading, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language__liturgical_preservation_reading, rope).
narrative_ontology:human_readable(hebrew_living_language__liturgical_preservation_reading, "Hebrew Living Language: Liturgical Preservation Reading").
narrative_ontology:topic_domain(hebrew_living_language__liturgical_preservation_reading, "sociolinguistics/language_revitalization/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_living_language__liturgical_preservation_reading, 'c2eecae7-f045-4803-9e29-c0b909f52331').
narrative_ontology:cs_kernel_codification('c2eecae7-f045-4803-9e29-c0b909f52331', fixed_text).
narrative_ontology:cs_authority_grounding('c2eecae7-f045-4803-9e29-c0b909f52331', lineage).
narrative_ontology:cs_interpretation_layer_present('c2eecae7-f045-4803-9e29-c0b909f52331').
narrative_ontology:cs_reading_relation('c2eecae7-f045-4803-9e29-c0b909f52331', hebrew_living_language__native_vernacular_reading, coexists_with).
narrative_ontology:cs_reading_relation('c2eecae7-f045-4803-9e29-c0b909f52331', hebrew_living_language__hybrid_continuity_reading, influences).
narrative_ontology:cs_axiom('c2eecae7-f045-4803-9e29-c0b909f52331', foundational, hebrew_sacredness_sufficient_for_continuity).
narrative_ontology:cs_axiom_status(hebrew_sacredness_sufficient_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('c2eecae7-f045-4803-9e29-c0b909f52331', hebrew_sacredness_sufficient_for_continuity, deontological).
narrative_ontology:cs_axiom('c2eecae7-f045-4803-9e29-c0b909f52331', foundational, textual_preservation_as_preservation_mechanism).
narrative_ontology:cs_axiom_status(textual_preservation_as_preservation_mechanism, holdable).
narrative_ontology:cs_axiom_grounding('c2eecae7-f045-4803-9e29-c0b909f52331', textual_preservation_as_preservation_mechanism, instrumental).
narrative_ontology:cs_reference_frame('c2eecae7-f045-4803-9e29-c0b909f52331', sacred_liturgical_hebrew_as_covenant_language).
narrative_ontology:cs_drift_state('c2eecae7-f045-4803-9e29-c0b909f52331', contemporary_secular_state_period, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c2eecae7-f045-4803-9e29-c0b909f52331', '').
narrative_ontology:cs_kernel_id(hebrew_living_language__liturgical_preservation_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_preservation_reading, liturgical_community).
narrative_ontology:constraint_beneficiary(hebrew_living_language__liturgical_preservation_reading, textual_authority_holders).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LITURGICAL COMMUNITY (ROPE) — Organized religious practitioners (synagogues, yeshivas, prayer communities) solve a genuine collective coordination problem: maintaining Hebrew as the sacred language of prayer requires synchronized memorization, ritual recitation, and textual transmission. The constraint functions as pure coordination — the community benefits from participation because unified prayer language enables shared spiritual practice. Low extraction; participants cannot exit without cost but perceive the constraint as enabling, not oppressive. The coordination function is genuine and asymmetric extraction is minimal.
constraint_indexing:constraint_classification(hebrew_living_language__liturgical_preservation_reading, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 2: TEXTUAL AUTHORITY (ROPE) — Institutions charged with Hebrew textual preservation (religious academies, scribal traditions, print houses, modern libraries) experience the constraint as legitimate coordination. The shared benefit is canonical textual stability — preserved texts enable all downstream religious and scholarly practice. The institutional actor benefits from preserving authoritative texts (gains prestige, authority to adjudicate correct forms) but this benefit is genuine coordination overhead, not extraction. The constraint solves the real problem of preventing textual decay and variant corruption.
constraint_indexing:constraint_classification(hebrew_living_language__liturgical_preservation_reading, rope,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: SECULARIZING SPEAKER (SNARE) — From the perspective of a Jew who wishes to speak Hebrew as a living vernacular but rejects its sacralization, the constraint is pure extraction. The liturgical preservation reading treats Hebrew as sacred text requiring ritual correctness, not as a language for daily life. Secularizers who want to use Hebrew as a vernacular tool face suppression: religious authority structures resist 'profane' uses, modern secular Hebrew is stigmatized within traditional communities, and the constraint's authority structure denies that Hebrew outside the sacred liturgical frame is 'real' Hebrew. The victim is trapped because exit (adopting another language) requires abandoning Hebrew identity entirely.
constraint_indexing:constraint_classification(hebrew_living_language__liturgical_preservation_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 4: BILINGUAL SPEAKER (ROPE) — A speaker fluent in both liturgical Hebrew and modern vernacular Hebrew experiences the constraint as coordination with friction. The liturgical preservation frame provides access to sacred texts, prayer community, and cultural continuity — genuine benefits. But the constraint also requires memorizing archaic forms, maintaining grammatical purity, and accepting restrictions on how Hebrew can be used. The speaker can exit by using only modern vernacular (constrained exit — social cost, loss of access to traditional community) but continues participating because the coordination benefits (spiritual access, cultural identity) exceed the cost. The constraint is rope, not snare, because exit is possible and some agents genuinely perceive net benefit.
constraint_indexing:constraint_classification(hebrew_living_language__liturgical_preservation_reading, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 5: ACADEMIC HEBRAIST (PITON) — Modern academic study of biblical and rabbinic Hebrew often reproduces the liturgical preservation frame (sacred texts require reverent study, linguistic purity is the marker of authenticity) without genuine functional need. Academic philology uses the ritualized textual practices as a marker of disciplinary legitimacy, not because sacred Hebrew requires this approach for prayer communities to function. The theater_ratio is high: much academic Hebrew study is performative adherence to a tradition whose primary function (religious liturgy) the academic context has abstracted away from. The piton classification reflects institutional inertia — preserving the sacred text protocols because they carry prestige and tradition, not because the functional coordination problem (maintaining a living liturgical language) is active in the academic context.
constraint_indexing:constraint_classification(hebrew_living_language__liturgical_preservation_reading, piton,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (ROPE) — From a civilizational analytical position, this reading models Hebrew language continuity as a genuine coordination mechanism that solves the real problem of maintaining a shared ritual language across time and dispersed communities. The constraint is rope because: (1) it enables genuine collective action (synchronized prayer), (2) extraction is minimal (preservation costs are coordination costs, not overhead), (3) the beneficiaries and participants substantially overlap. The analytical observer recognizes this reading as one legitimate frame among three competing readings, each with different structural properties.
constraint_indexing:constraint_classification(hebrew_living_language__liturgical_preservation_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language__liturgical_preservation_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_living_language__liturgical_preservation_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_living_language__liturgical_preservation_reading, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_living_language__liturgical_preservation_reading, TR),
    TR >= 0.70.

:- end_tests(hebrew_living_language__liturgical_preservation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.18): Low. The liturgical preservation reading minimizes extraction because the coordination function is genuine and primary. Hebrew textual preservation solves a real problem: maintaining canonical forms across time and dispersed communities. The institutions that preserve texts (scribal traditions, yeshivas, later print authority) benefit from prestige and authority, but these benefits are coordination overhead, not extractive premium. There is no dominated class bearing costs while beneficiaries capture surplus — the beneficiaries (liturgical community, text authorities) are substantially the same as participants (people doing the coordination work). Extraction would only arise if secular speakers were forced to participate in the constraint against their will, but the rope classification means their exit option is available at moderate cost. Suppression (0.32): Moderate. Barriers to exit from the constraint include: (1) cultural-identity factors (Hebrew is sacred; secularizing it feels like identity betrayal), (2) institutional authority (religious authorities delegitimize secular uses), (3) social stigma (violating linguistic purity norms carries shame in traditional communities). But suppression is not total — secular Hebrew speakers do exit and build vernacular communities; the Israeli state did develop modern Hebrew despite religious opposition. Theater ratio (0.55): Moderate. Ritual recitation includes authentic functional elements (real coordination benefit from synchronized prayer) plus performative elements (precision of ancient pronunciation matters primarily for ritual correctness, not linguistic function; extensive memorization of archaic forms serves tradition transmission more than contemporary communication). The theater has risen from historical baseline (0.42) as academic study has abstracted the sacred protocols from their liturgical function.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a significant perspectival gap between the liturgical community's rope classification and the secularizing speaker's snare classification. This gap reveals the core tension in this reading: liturgical preservation treats Hebrew as a sacred coordination mechanism (rope) while simultaneously suppressing vernacular use as 'profane' (snare from the secularizer's perspective). The institutional beneficiary (text authorities) see rope; the powerless victim (secularizer) sees snare. The piton classification of academic Hebraism shows how the same institutional form (sacred text preservation protocols) can degrade from rope (when serving liturgical coordination) to piton (when abstracted into academic ritual) as the functional need disappears. The analytical observer's rope classification is actually analytically different from the liturgical community's rope — it classifies based on the genuine coordination function, not on lived experience. The bilingual speaker's rope classification reflects their actual structural position (moderate extraction from liturgical frames offset by vernacular benefits), making it empirically distinct from the secularizer's snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) flow from the structural relationship of each agent to the constraint. Liturgical community members are net beneficiaries with constrained exit (moderate power, generational horizon, constrained options, regional scope) — they perceive genuine coordination benefit. Institutional text authorities are beneficiaries with arbitrage options (institutional power, civilizational horizon, arbitrage exit, global scope) — they have strongest net benefit and easiest exit, yet remain because prestige and authority justify participation. Secularizing speakers are victims with no exit in the constraint itself (powerless, biographical horizon, trapped exit, local scope) — they either accept the sacred frame or abandon Hebrew entirely. Bilingual speakers are mixed (moderate power, biographical horizon, constrained exit, regional scope) — they experience both coordination benefits and suppression costs. Academic Hebraists are institutional beneficiaries with arbitrage (institutional power, biographical horizon, arbitrage exit, regional scope) — they benefit from prestige but the functional need (liturgical coordination) is absent in their context, producing the piton classification. The analytical observer (analytical power, civilizational horizon, analytical exit, global scope) recognizes the rope classification as structurally accurate for the functional coordination problem this reading solves.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sacred_versus_living_definition,
    'What constitutes a ''living language'' under the liturgical preservation reading — does active ritual use count as living, or is native intergenerational vernacular transmission required?',
    'Linguistic anthropology comparative analysis: examine other liturgical languages (Latin in Catholicism, Classical Arabic in Islamic contexts, Sanskrit in Hindu ritual) and their classification as living or dead. Compare functional criteria across religious and secular language preservation communities.',
    'If ritual use counts as living: this reading is structurally stable (rope, ε=0.18). If native vernacular is required: this reading forecloses itself — Hebrew cannot be living under purely liturgical preservation. If hybrid (ritual + some vernacular) is required: reading coexists with native_vernacular_reading with resource competition (influences relation rather than coexists_with).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sacred_versus_living_definition, conceptual, 'Definition of ''living language'' under liturgical preservation frame').

omega_variable(
    secular_hebraism_as_victim_or_beneficiary,
    'Is modern secular Hebrew (Israeli vernacular) a victim group oppressed by the liturgical preservation frame, or a beneficiary that leverages sacred textual authority for nation-building?',
    'Historical institutional analysis: trace how Zionist movement appropriated liturgical Hebrew authority to legitimize modern Hebrew development; identify periods when secular authorities explicitly rejected vs. appropriated sacred linguistic frames; analyze funding and institutional support for secular vs. liturgical Hebrew education.',
    'If secular Hebrew is primarily victim (cultural suppression): snare classification strengthens, suppression value rises, victim set expands. If secular Hebrew is beneficiary (exploiting sacred authority): extractiveness rises (0.18 → 0.35+), and ε-value shifts toward tangled_rope. If both simultaneously (dual character): a structural decomposition is warranted — write separate stories for liturgical_preservation and secular_appropriation constraints.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secular_hebraism_as_victim_or_beneficiary, empirical, 'Structural relationship between secular Hebrew and liturgical preservation frame').

omega_variable(
    kernel_occupation_sufficiency,
    'Does this reading''s claim that Hebrew ''lives'' through symbolic preservation without native speakers occupy the same kernel as the native_vernacular_reading''s claim that Hebrew requires daily speech?',
    'Examine whether both readings could be held simultaneously within the same institutional or community framework. Test by: (1) analyzing communities that claim both positions (Israeli religious education systems), (2) identifying logical entailments (does accepting liturgical preservation logically commit one to accepting or rejecting native vernacular?), (3) mapping historical moments of switching between readings.',
    'If readings are logically incompatible within a single framework: forecloses relation. If communities hold both sequentially or in different domains: coexists_with relation. If this reading creates conditions that make the native vernacular reading possible (hybrid_continuity): influences relation (downstream causal structure). Impact on network topology and constraint family structure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_occupation_sufficiency, conceptual, 'Whether liturgical and native readings occupy the same contested kernel').

omega_variable(
    textual_corruption_risk_over_literacy_decline,
    'In long historical perspective, does the constraint''s primary function shift from maintaining textual purity (against scribal errors) to maintaining any Hebrew literacy at all (against total language death)?',
    'Longitudinal textual variance analysis: compare error rates and variant distribution across manuscript traditions at different historical periods. Correlate with general literacy rates, institutional preservation capacity, and demographic changes in Hebrew-reading populations.',
    'If textual purity remains primary: extraction stays low (ε=0.18), rope classification holds. If constraint becomes primarily literacy-maintenance: the coordination function expands, potentially strengthening rope classification. If literacy maintenance fails: constraint may drift toward piton (performative ritual without real function). Theater_ratio and measurement trajectory should reflect this shift.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_corruption_risk_over_literacy_decline, empirical, 'Shift in constraint''s functional focus over historical time').

omega_variable(
    reading_kernel_boundary_contest,
    'Is the kernel ''Hebrew lives'' (abstract linguistic continuity) or is it ''Hebrew lives through sacred recitation specifically'' (constraining to liturgical mode)? Does this reading define the kernel or merely read it?',
    'Examine the historical moment when ''Hebrew'' became contested (post-70 CE diaspora, Enlightenment, Zionist movement): what were the competing claims about what it means for a language to ''live''? Identify which claims counted as readings of a shared kernel vs. claims that redefined the kernel entirely.',
    'If this reading defines part of the kernel (liturgical preservation IS part of what the kernel says): the reading_relations edges change from coexists_with to influences or forecloses. If this reading is merely one reading of a broader kernel: current relations hold. Impacts whether other readings are properly sibling readings or constitute different kernels.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_boundary_contest, conceptual, 'Whether this reading defines or merely reads the contested kernel').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language__liturgical_preservation_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t0, hebrew_living_language__liturgical_preservation_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(hebr_tr_t3, hebrew_living_language__liturgical_preservation_reading, theater_ratio, 3, 0.48).
narrative_ontology:measurement(hebr_tr_t6, hebrew_living_language__liturgical_preservation_reading, theater_ratio, 6, 0.55).

% Extraction over time
narrative_ontology:measurement(hebr_be_t0, hebrew_living_language__liturgical_preservation_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(hebr_be_t3, hebrew_living_language__liturgical_preservation_reading, base_extractiveness, 3, 0.15).
narrative_ontology:measurement(hebr_be_t6, hebrew_living_language__liturgical_preservation_reading, base_extractiveness, 6, 0.18).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language__liturgical_preservation_reading, identity_coordination).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_preservation_reading, hebrew_living_language__native_vernacular_reading).
narrative_ontology:affects_constraint(hebrew_living_language__liturgical_preservation_reading, hebrew_living_language__hybrid_continuity_reading).

% DUAL FORMULATION NOTE:
% The kernel 'hebrew_lives' decomposes into three structurally distinct constraints with different ε values and beneficiary/victim structures. Each reading is a separate constraint story. This story (liturgical_preservation_reading) has ε≈0.18 (rope, pure coordination). The native_vernacular_reading should have higher extractiveness (snare risk: native speakers as elite class, vernacular revival as status marker). The hybrid_continuity_reading occupies intermediate ε (tangled rope: genuine liturgical coordination plus vernacular aspiration with competing resource claims). All three are linked via network.affects_constraints to form a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
