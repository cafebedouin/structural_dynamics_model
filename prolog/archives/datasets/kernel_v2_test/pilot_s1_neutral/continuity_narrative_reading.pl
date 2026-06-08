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
 *   human_readable: Hebrew Continuity Narrative Reading: Restoration as Natural State
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   This constraint instantiates ONE READING of the contested Hebrew language
 *   kernel: the continuity_narrative_reading, which frames Hebrew revival as
 *   restoration of an unbroken natural state rather than artificial creation.
 *   This reading operates through a legitimacy claim about Hebrew's
 *   historical trajectory: despite 2,000 years of diaspora, Hebrew maintained
 *   an unbroken tradition through liturgical and literary transmission, and
 *   modern revival is the restoration of that tradition to spoken use. The
 *   reading benefits the Israeli national institutional order and nationalist
 *   legitimacy projects by providing historical justification for territorial
 *   and cultural claims rooted in Hebrew as the 'language of the land.' It
 *   suppresses alternative readings that would acknowledge the constructed,
 *   innovative, and deliberately designed aspects of modern Hebrew grammar
 *   and vocabulary. The constraint exhibits tangled_rope structure: genuine
 *   coordination function (revival communities coordinated linguistic
 *   innovation and pedagogy) coupled with asymmetric extraction (the
 *   coordination work is subordinated to a naturalizing narrative that
 *   attributes it to unbroken tradition rather than collective effort). The
 *   theater ratio (0.68) reflects increasing reliance on nationalist
 *   narrative rather than actual historical transmission mechanisms.
 *   Suppression (0.58) indicates moderate-to-high institutional gatekeeping
 *   of counter-narratives. Extractiveness (0.52) reflects that the
 *   constraint's primary function has shifted from coordination (solving the
 *   problem of creating a spoken modern Hebrew) to legitimation (justifying
 *   the national project).
 *
 * KEY AGENTS:
 *   - Israeli National Institutional Order: Primary beneficiary (institutional/arbitrage) — gains legitimacy foundation for nation-state through continuity narrative vindication
 *   - Revival Movement Scholars & Educators: Secondary beneficiary (institutional/constrained) — gained institutional positions and authority through revival expertise; constrained by identity fusion with continuity narrative
 *   - Counter-Narrative Scholars: Primary victim (moderate/identity_locked) — face suppression through career barriers, publication gatekeeping, identity-lock mechanisms that make dissent feel like national betrayal
 *   - Linguistic Historicity (abstract): Victim (powerless/trapped) — suppressed without benefit; historical accuracy about construction mechanisms cannot be articulated within the frame
 *   - Diaspora Hebrew Literary Tradition: Secondary agent (institutional/analytical) — maintains actual continuity but is theatrically invoked rather than functionally central to modern Hebrew
 *   - Analytical Observer (Cross-Reading): Seat-neutral (analytical/analytical) — can recognize the continuity narrative as a reading among siblings rather than as transcendent natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_narrative_reading, 0.52).
domain_priors:suppression_score(continuity_narrative_reading, 0.58).
domain_priors:theater_ratio(continuity_narrative_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_narrative_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(continuity_narrative_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(continuity_narrative_reading, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_narrative_reading, tangled_rope).
narrative_ontology:human_readable(continuity_narrative_reading, "Hebrew Continuity Narrative Reading: Restoration as Natural State").
narrative_ontology:topic_domain(continuity_narrative_reading, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(continuity_narrative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_narrative_reading, 'db879668-053c-42ff-932e-56188726e4e8').
narrative_ontology:cs_kernel_codification('db879668-053c-42ff-932e-56188726e4e8', fixed_text).
narrative_ontology:cs_authority_grounding('db879668-053c-42ff-932e-56188726e4e8', extraction).
narrative_ontology:cs_interpretation_layer_present('db879668-053c-42ff-932e-56188726e4e8').
narrative_ontology:cs_reading_relation('db879668-053c-42ff-932e-56188726e4e8', continuity_narrative_reading__hebrew_liturgical_reading, coexists_with).
narrative_ontology:cs_reading_relation('db879668-053c-42ff-932e-56188726e4e8', continuity_narrative_reading__hebrew_native_daily_reading, influences).
narrative_ontology:cs_axiom('db879668-053c-42ff-932e-56188726e4e8', foundational, hebrew_unbroken_natural_continuity).
narrative_ontology:cs_axiom_status(hebrew_unbroken_natural_continuity, holdable).
narrative_ontology:cs_axiom_grounding('db879668-053c-42ff-932e-56188726e4e8', hebrew_unbroken_natural_continuity, empirically_contingent).
narrative_ontology:cs_axiom('db879668-053c-42ff-932e-56188726e4e8', foundational, restoration_not_construction_doctrine).
narrative_ontology:cs_axiom_status(restoration_not_construction_doctrine, overridden).
narrative_ontology:cs_axiom_grounding('db879668-053c-42ff-932e-56188726e4e8', restoration_not_construction_doctrine, instrumental).
narrative_ontology:cs_reference_frame('db879668-053c-42ff-932e-56188726e4e8', hebrew_unbroken_diaspora_transmission).
narrative_ontology:cs_drift_state('db879668-053c-42ff-932e-56188726e4e8', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('db879668-053c-42ff-932e-56188726e4e8', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(continuity_narrative_reading, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, israeli_national_legitimacy_project).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, revival_institutional_establishment).
narrative_ontology:constraint_victim(continuity_narrative_reading, counter_narrative_suppression).
narrative_ontology:constraint_victim(continuity_narrative_reading, linguistic_historicity).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, revival_era_educators_scholars).
narrative_ontology:constraint_beneficiary(continuity_narrative_reading, modern_hebrew_speakers).
narrative_ontology:constraint_victim(continuity_narrative_reading, historical_linguists_counter_narrative).
narrative_ontology:constraint_vindicates(continuity_narrative_reading, hebrew_unbroken_cultural_continuity).
narrative_ontology:constraint_vindicates(continuity_narrative_reading, restoration_not_construction_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the narrative frame through official education, media, and cultural institutions. Enforces continuity narrative through publication gatekeeping, institutional appointments, and national identity rhetoric. Benefits from the narrative's legitimacy contribution to territorial and statehood claims. Can arbitrage to alternative narratives if needed (though at substantial political cost).
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, israeli_state_institutional_order, agenda_setter,
    institutional, generational, arbitrage, national).

% Created modern Hebrew through pedagogical innovation and linguistic labor. Benefit from institutional positions and authority as revival experts. Constrained by identity fusion with the national continuity narrative — dissenting from continuity would undermine their authority as legitimate experts. Both coordinate the genuine revival work and enforce the naturalizing narrative that subordinates their agency.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, revival_era_educators_scholars, beneficiary,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(continuity_narrative_reading, revival_era_educators_scholars, agenda_setter).

% Scholars who recognize and document the constructed, innovative aspects of modern Hebrew (neologism creation, grammatical innovation, deliberate design choices by Revival educators). Face career sanctions, publication barriers, and delegitimization as 'enemies of the language' within Israeli institutions. Identity-locked: dissent feels like national betrayal. Constrained by institutional power asymmetry — counter-narrative scholarship can be published internationally but faces gatekeeping within Israeli academic and educational systems.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, historical_linguists_counter_narrative, payer,
    moderate, biographical, identity_locked, global).

% Maintained actual Hebrew literacy and literary practice through 2,000 years of diaspora. The empirical basis for the 'unbroken tradition' claim. However, marginalized in modern continuity narrative — the specific historical transmission of diaspora communities is subsumed under abstract 'tradition.' Their lived experience of Hebrew practice does not map onto the modern language (different phonology, grammar, vocabulary choices). Trapped by their position as witnesses to actual continuity whose testimony about differences between diaspora and modern Hebrew is suppressed.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, diaspora_hebrew_communities, observer,
    powerless, generational, trapped, global).
narrative_ontology:stakeholder_secondary_role(continuity_narrative_reading, diaspora_hebrew_communities, excluded).

% The abstract collective good of linguistic historical accuracy. Bears the cost of the continuity narrative's suppression of construction mechanisms and innovation. Non-agent (a proposition rather than a person), so excluded from beneficiary/victim derivation, but included in narrative to document what is suppressed.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, linguistic_historicity_abstract, payer,
    powerless, civilizational, trapped, global).
narrative_ontology:stakeholder_non_agent(continuity_narrative_reading, linguistic_historicity_abstract).

% Speakers benefit from having a functional modern language created through Revival work. Trapped by the language's institutional status and by identity fusion with the national narrative that frames their language as unbroken continuation rather than recent creation. Benefits are real (functional language for communication and national identity) but extraction occurs through the suppression of the agency and innovation that created the language.
narrative_ontology:constraint_stakeholder(continuity_narrative_reading, modern_hebrew_speakers, beneficiary,
    powerless, immediate, trapped, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Hebrew-speaking communities needed a shared modern language with institutional legitimacy and psychological continuity to sustain national identity and community cohesion across the transition from diaspora to modern statehood. The continuity narrative provided that psychological continuity by framing modern Hebrew as restoration of an ancient language rather than invention of a new one.
% TRANSFER_FUNCTION: The constraint transfers historical legitimacy and national identity resources FROM counter-narrative scholars, historical accuracy, and actual innovation records TO nationalist institutional projects and the state's territorial and cultural legitimacy claims. The constraint also transfers suppression FROM institutional gatekeepers TO scholars who would document the constructed aspects of modern Hebrew.
% ABSENT_VOICES: Diaspora Hebrew communities whose lived experience of Hebrew practice differs from modern Hebrew norms are structurally absent from the continuity narrative frame — their testimony about differences is suppressed. Post-Zionist scholars, Israeli linguistic minorities, and international linguistic historians who recognize construction mechanisms are excluded from mainstream Israeli institutional discourse about Hebrew's nature. Non-nationalist educational models that would frame Hebrew as a deliberately created language rather than a restored one have no seat at the table.
% DISAPPEARANCE_RATIONALE: If the continuity_narrative_reading constraint disappeared overnight, Israeli national identity and territorial legitimacy claims would be forced to rest on alternative foundations (political choice, historical presence, contemporary state power) rather than on linguistic and historical continuity. The constraint's disappearance would not eliminate Hebrew — speakers would persist, the language would function — but it would transform the narrative frame in which Hebrew's status and legitimacy are understood. Institutional actors (Israeli state, academic establishments) would claim the constraint is indispensable to national coherence; counter-narrative scholars would argue the constraint's disappearance would enable more accurate historical understanding without harming the language itself. The verdict is contested because the constraint's function is partly coordination (supporting Hebrew community) and partly legitimation (supporting state claims), and these functions are bundled together in the narrative.
% FOUNDING_PROBLEM: Early 20th-century Hebrew revival communities faced the problem of creating a modern Hebrew-speaking population and community. Hebrew had persisted in liturgy and literature but not as a daily spoken language. Speakers needed a shared modern language with shared norms, vocabulary for contemporary concepts, and institutional legitimacy. The continuity narrative was instrumental to solving this problem: framing the new language as restoration rather than invention provided psychological continuity and institutional legitimacy that enabled speakers and educators to embrace the language.
% FOUNDING_PROBLEM_CORROBORATION: Historical linguists (Spolsky, Rabin, Koro'a, modern historical linguistics scholarship) document that the founding problem (creating modern speakers) was live through mid-20th century and is now resolved. Revival-era documents and educators' writings corroborate the original coordination goal. Israeli education system data corroborate that Hebrew transmission has shifted from revival pedagogy to native-speaker acquisition (a sign that the founding coordination problem is solved). International scholars outside nationalist frameworks corroborate the dead status of the founding problem.
narrative_ontology:disappearance_verdict(continuity_narrative_reading, contested).
narrative_ontology:founding_problem_status(continuity_narrative_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LINGUISTIC HISTORICITY (SNARE) — Cannot contest the continuity narrative without being positioned as hostile to national identity. Historical accuracy (Hebrew's 19th-century reconstruction, the genuine liturgical-to-spoken transition, the role of constructed neologisms) is suppressed through identity fusion: acknowledging the constructed nature feels like denying the legitimacy of the nation itself. Structurally mobile exit (academic dissent, counter-scholarship) but identity-locked from within the framework — exit would require abandoning one's position as a legitimate scholar within the national project. Full victim status: bears suppression without benefit from the constraint.
constraint_indexing:constraint_classification(continuity_narrative_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 2: HEBREW REVIVAL COMMUNITY (TANGLED ROPE) — Coordinated the genuine revival work (pedagogical innovation, neologism creation, colloquialization of liturgical forms). Genuine coordination function: speakers needed a shared modern language. Asymmetric extraction: the community's actual labor and innovation are subordinated to a naturalizing narrative that attributes revival to unbroken tradition rather than sustained collective effort. Constrained exit: breaking from continuity narrative threatens both legitimacy of the revival project and individual speakers' national identity. Medium experienced extraction.
constraint_indexing:constraint_classification(continuity_narrative_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: ISRAELI NATIONAL INSTITUTIONAL ORDER (ROPE) — Benefits from continuity narrative as legitimacy foundation for the nation-state. The claim 'Hebrew is restored, not invented' vindicates national historical claims and provides continuity justification for territorial claims. Experiences the constraint as pure coordination: it solves the problem of national-identity coherence and cultural legitimacy. High arbitrage exit options: the institutional order can shift legitimacy frames if needed (though at high cost). Net beneficiary perspective: extraction runs toward this agent through vindication of foundational doctrines.
constraint_indexing:constraint_classification(continuity_narrative_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ACADEMIC HEBREW ESTABLISHMENT (TANGLED ROPE) — Coordinates genuine linguistic scholarship and pedagogy; benefits from institutional positions and funding tied to the revival narrative. Asymmetric extraction: scholars who acknowledge the constructed/innovative aspects of modern Hebrew face career risk, publication barriers, and delegitimization as 'enemies of the language.' Constrained exit: dissenting from continuity narrative risks losing institutional standing. Coordination function is real (teaching Hebrew, training speakers, developing linguistic norms); extraction mechanism is the suppression of counter-narrative scholarship that would complicate the continuity claim.
constraint_indexing:constraint_classification(continuity_narrative_reading, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: DIASPORA HEBREW LITERARY TRADITION (PITON) — Historically maintained liturgical and literary Hebrew across 2,000 years; this actual continuity ground the continuity narrative's plausibility. But the modern continuity narrative has largely replaced the diaspora tradition's role: contemporary Hebrew speakers and institutions invoke abstract 'unbroken tradition' rather than the specific lineage of Hebrew literature and liturgy. The diaspora tradition's actual transmission mechanism (rabbinical education, literary preservation, liturgical practice) is theatrically invoked but functionally superseded by modern institutional revival. The real continuity carrier (diaspora textual communities) is maintained as historical reference but not as the operative mechanism of modern Hebrew. Theater ratio 0.68 reflects this: the constraint's legitimacy increasingly derives from nationalist narrative rather than from the actual historical transmission it once denoted.
constraint_indexing:constraint_classification(continuity_narrative_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / FALSE SUMMIT VIEW (MOUNTAIN) — From a civilizational scope, the claim 'Hebrew continuity is unbroken natural state' appears as an immutable fact about how languages persist through diaspora. The continuity narrative rests on genuine historical facts: Hebrew liturgy and literature DID persist unbroken. However, the critical equivocation lies in the gap between 'liturgical Hebrew persisted unbroken' (true, mountain-grade) and 'modern spoken Hebrew is the natural restoration of that unbroken tradition' (contingent, constructed). This perspective conflates the two claims into a single naturalizing narrative. The engine's false summit detector will identify this: beneficiary presence + vindicated propositions + institutional enforcement + measured extraction suggest this is a reading-specific legitimacy claim, not a natural law.
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
 *   Extractiveness (0.52): Moderate. The reading extracts substantial legitimacy and institutional resources for nationalist projects. However, the extraction is partly offset by genuine coordination benefits — revival communities did successfully create a spoken modern Hebrew, which is a real achievement. The 0.52 value reflects that the constraint's function has shifted over the interval from coordination (t=0, extractiveness 0.35) to legitimation (t=20, extractiveness 0.52). The extraction increase represents the rising importance of the continuity narrative as a legitimacy foundation relative to the original coordination problem (creating modern speakers). Suppression (0.58): Moderate-to-high. Counter-narratives about Hebrew's constructed aspects face institutional gatekeeping, publication barriers, and identity-fusion mechanisms that suppress dissent. Scholars who acknowledge construction risk delegitimization. However, suppression is not total — international historical linguistics scholarship exists and is occasionally available. The 0.58 value reflects this partial suppression. Suppression increases over the interval (0.32 to 0.58) as the continuity narrative becomes more institutionalized and national identity becomes more explicitly tied to language continuity claims. Theater ratio (0.68): High and rising. Modern continuity-narrative operation increasingly relies on performative invocation of 'unbroken tradition' rather than on actual genealogical transmission mechanisms. The diaspora literary tradition (the actual continuity carrier) is theatrically referenced but functionally replaced by nationalist narrative. Early revival (t=0) was more functionally focused on solving the practical problem of creating modern speakers; contemporary operation (t=20) increasingly centers legitimacy performance and identity affirmation.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is the core diagnostic for this reading. The national institutional order sees rope: a coordination mechanism solving the problem of national identity coherence through historical legitimacy narratives. The revival community sees tangled_rope: genuine coordination work coupled with subordination of that work to a naturalizing narrative that erases their agency. Counter-narrative scholars see snare: trapped by identity-lock into acceptance of the continuity frame while watching historical accuracy suppressed. The diaspora tradition sees piton: their actual work of transmission is theatrically invoked but functionally replaced. The analytical observer risks seeing mountain: treating 'Hebrew continuity is unbroken' as a natural law rather than recognizing it as a reading-specific legitimacy claim. The gap between national institutional perspective (rope) and counter-narrative perspective (snare) is maximal: the same constraint is experienced as pure benefit (coordination) from the seat that collects and as pure cost (suppression) from the seat that bears suppression. This gap reveals the constraint's extraction mechanism: the continuity narrative solves a coordination problem (national identity) for beneficiaries while suppressing alternative frames that would complicate the solution.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by its structural position relative to the constraint. The Israeli institutional order (institutional/arbitrage) experiences the continuity narrative as pure coordination solving a legitimacy problem — high arbitrage exit means low effective extraction chi. The national institutional perspective benefits from the constraint and experiences it as a natural-law-like background to national identity — directionality d ≈ 0.0, negative chi (net subsidy to this actor). The counter-narrative scholars (moderate/identity_locked) have structural mobility (could leave academic positions, publish outside national frameworks) but are identity-locked — they cannot exit the constraint without abandoning their position as legitimate scholars within the national project. This identity-lock magnifies experienced extraction: d ≈ 0.8-0.9, high chi. The revival community (institutional/constrained) benefits from the constraint through institutional positions but is constrained by identity fusion with the continuity narrative — d ≈ 0.45-0.55, moderate chi reflecting the tangled-rope mixed experience. The linguistic historicity (powerless/trapped) has no exit option and no benefit — d ≈ 1.0, maximum chi, classifying as snare from this perspective.
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL READING MANDATROPHY DIAGNOSIS: This reading exhibits classic mandatrophy structure. The founding problem was real and live: Hebrew-speaking revival communities needed a modern language and linguistic legitimacy in the early 1900s (t ≈ 0). The coordinate solution was genuine: revival educators, scholars, and communities did create modern Hebrew through deliberate linguistic work. The continuity narrative was instrumentally useful: framing the revival as restoration rather than construction provided historical legitimacy and psychological continuity that enabled speakers and institutions to embrace the new language. By t ≈ 10-15, the founding problem had substantially resolved — Hebrew-speaking communities existed, the language was institutionalized, and the revival goal was achieved. However, the continuity narrative persisted and transformed: it shifted from instrumental coordination support (solving the problem of creating speakers) to legitimacy foundation (justifying nationalist territorial and cultural claims). At t=20, the constraint has become largely performative: the continuity narrative is maintained through institutional enforcement and identity-lock, not through functional necessity. The founding problem (needing a spoken modern language) is dead, but the constraint persists because it now serves a different mandate: providing historical legitimacy for the national project. This is textbook mandatrophy: the constraint's original coordination function has atrophied, but the constraint is maintained through institutional inertia and increasingly through performative theater (theater_ratio rising from 0.42 to 0.68). The extraction increases as the theater increases because the constraint now serves primarily to extract legitimacy for nationalist projects rather than to coordinate speakers.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_innovation_boundary,
    'What empirical or conceptual facts would establish whether modern Hebrew is continuity with the diaspora tradition versus novel construction drawing on diaspora resources?',
    'Comparative analysis of: (1) neologism frequency and acceptance mechanisms in modern Hebrew vs. diachronic Hebrew texts; (2) grammatical innovation vs. conservative preservation in Modern Hebrew phonology, morphology, syntax; (3) speaker metalinguistic claims about ''restoration'' vs. historical linguist assessment of innovation percentage; (4) institutional documentation of Revival-era deliberate language design choices.',
    'If continuity dominates: the reading is empirically vindicated and mountain risk declines. If innovation dominates: the reading is a constructed legitimacy claim, and reclassification from false summit to explicit tangled_rope becomes justified. The boundary is the contested locus of the kernel reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_innovation_boundary, empirical, 'Empirical threshold for distinguishing continuity from construction').

omega_variable(
    reading_specificity_vs_natural_law,
    'Does the continuity narrative instantiate a coherent reading of the Hebrew language kernel (a legitimate frame among multiple possible frames), or does it naturalize contingent historical outcomes as unchangeable laws?',
    'Test: (1) Can a coherent counter-reading (liturgical_reading, native_daily_reading) be articulated without internal contradiction? (2) Do sibling readings occupy different institutional seats or do they attempt to occupy the same seat? (3) Is the continuity narrative preserved through deliberate institutional enforcement (gate closure, suppression of counter-narratives) or through genuine epistemic consensus? If (1) yes AND (2) different seats AND (3) yes to enforcement, then this is a reading-specific legitimacy claim that benefits from naturalization. If (1) no OR (2) single seat OR (3) no enforcement, then continuity might be a genuine natural law about how languages persist.',
    'If reading-specific: false summit certification and explicit extraction mechanism expose the suppression structure. If natural law: the engine''s naturality gates pass and the constraint is reclassified as mountain. This omega determines the critical watershed between ''reading among siblings'' and ''natural fact''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_specificity_vs_natural_law, conceptual, 'Whether continuity narrative is reading-specific frame or transcendent natural law').

omega_variable(
    identity_lock_mechanism_in_suppression,
    'Is the suppression of counter-narratives primarily structural (institutional gatekeeping, publication barriers, career sanctions) or primarily internalized (scholars'' identity fusion with the national continuity project preventing them from articulating counter-narratives)?',
    'Post-suppression analysis: (1) Do scholars who leave institutional Hebrew contexts (emigration, career changes) become able to articulate counter-narratives? (2) Do institutions that adopt counter-narratives (diaspora academies, non-nationalist linguistic societies) face external sanctions or internal epistemic conviction? (3) Historical documentation of scholars who shifted from continuity advocacy to continuity critique and their reported reasons (career change, identity shift, theoretical framework change, external pressure).',
    'If primarily structural: suppression is a gatekeeping mechanism that external pressure or institutional reform could dismantle. If primarily internalized: suppression persists even after structural barriers fall, and counter-narrative suppression is a property of the reading itself, not of external enforcement. Internalized suppression suggests higher effective extraction from the constraint''s perspective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_in_suppression, empirical, 'Structural versus internalized suppression mechanism').

omega_variable(
    kernel_reading_contest_locus,
    'What specific claims about the Hebrew kernel are in contest between this reading and its siblings, and are those claims about the same kernel or different kernels?',
    'Explicit mapping of the reading''s axioms to sibling readings'' axioms and their logical relationships. Does the continuity_narrative_reading claim that ''Hebrew unbroken tradition persisted'' while liturgical_reading claims ''Hebrew was a liturgical artifact'' — same kernel, different readings? Or do they describe different kernels entirely (the liturgical object vs. the living language object)? If same kernel: reading_relations are coherent (coexists_with or forecloses). If different kernels: the contest is not between readings but between two separate constraints that should be decomposed.',
    'If same kernel: the reading structure is valid and siblings can coexist or foreclose each other coherently. If different kernels: this constraint story should be decomposed into separate files per the ε-invariance principle, and the ''contest'' is actually a constraint family rather than a reading contest. The kernel''s granularity is the unit of reading analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_contest_locus, conceptual, 'Whether sibling readings contest the same kernel or different kernels').

omega_variable(
    nationalism_extraction_vector,
    'Is the beneficiary of this constraint the Israeli national institutional order itself, or are the beneficiaries more precisely framed as political elites and nationalist ideological projects that use the continuity narrative to justify territorial and cultural claims?',
    'Structural analysis: (1) Who specifically collects from the continuity narrative — state institutions, ideological parties, institutional Hebrew establishments, or the nation as abstract entity? (2) Are there Israeli institutional actors (e.g., diaspora communities, linguistic minorities, post-Zionist scholars) who do NOT benefit from the continuity narrative, and if so, what is their structural relationship to the constraint? (3) Do beneficiaries from the continuity narrative compete with each other for legitimacy, or do they form a unified coalition?',
    'Granular beneficiary identification allows finer-grained directionality derivation. If the beneficiary is homogeneous (national institutional order), then d for the institutional perspective is well-defined. If the beneficiary is fragmented (competing nationalist projects with different legitimacy frames), then multiple institutional perspectives may be needed, each with different d values and potential classifications.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nationalism_extraction_vector, empirical, 'Specificity of beneficiary identification and coalition structure').

omega_variable(
    historical_authenticity_gap,
    'At what point in the Revival process did the continuity narrative become operational as a legitimacy claim versus a descriptive historical fact? Was the narrative constructed retroactively to justify choices already made, or was it predictive guidance for Revival work?',
    'Archival analysis of Revival-era writings: (1) Do early Revival texts (1880s–1920s) frame Hebrew revival as ''restoration of unbroken tradition'' or as ''innovative revival of an ancient language''? (2) When does the continuous-tradition narrative appear in institutional documents, educational materials, and ideological texts? (3) Do scholars and Revival leaders shift their language over time from ''construction'' to ''restoration,'' and if so, what events or pressures correspond to the shift?',
    'If narrative is retroactive: the constraint becomes an explicit legitimacy overlay on actual construction work, increasing evidence for tangled_rope classification and reducing mountain risk. If narrative is predictive: the constraint embeds an expectation structure that shaped Revival work from its inception, raising the possibility that ''restoration'' was not purely constructed but guided by and through the continuous-tradition frame. The timing of narrative adoption is evidence for mandatrophy analysis: does the Revival''s founding problem (creating a modern Hebrew-speaking community) remain live, or has it been displaced by the continuity narrative''s new function (providing national legitimacy)?',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(historical_authenticity_gap, empirical, 'Temporal location of continuity narrative adoption as legitimacy mechanism').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_narrative_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_narr_tr_t0, continuity_narrative_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cont_narr_tr_t10, continuity_narrative_reading, theater_ratio, 10, 0.58).
narrative_ontology:measurement(cont_narr_tr_t20, continuity_narrative_reading, theater_ratio, 20, 0.68).

% Extraction over time
narrative_ontology:measurement(cont_narr_be_t0, continuity_narrative_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(cont_narr_be_t10, continuity_narrative_reading, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(cont_narr_be_t20, continuity_narrative_reading, base_extractiveness, 20, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(cont_narr_su_t0, continuity_narrative_reading, suppression_requirement, 0, 0.32).
narrative_ontology:measurement(cont_narr_su_t10, continuity_narrative_reading, suppression_requirement, 10, 0.48).
narrative_ontology:measurement(cont_narr_su_t20, continuity_narrative_reading, suppression_requirement, 20, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_narrative_reading, identity_coordination).
narrative_ontology:affects_constraint(continuity_narrative_reading, hebrew_liturgical_reading).
narrative_ontology:affects_constraint(continuity_narrative_reading, hebrew_native_daily_reading).
narrative_ontology:affects_constraint(continuity_narrative_reading, israeli_territorial_legitimacy).
narrative_ontology:affects_constraint(continuity_narrative_reading, diaspora_hebrew_transmission).

% DUAL FORMULATION NOTE:
% The continuity_narrative_reading is one component of a constraint family centered on the hebrew_living_language kernel. All three readings (continuity_narrative, liturgical, native_daily) describe the same historical phenomenon (Hebrew's status as a language across diaspora and revival) but instantiate different constraints with different ε values and beneficiary structures. The continuity_narrative_reading exhibits moderate-high extractiveness (0.52) because it benefits nationalist legitimacy projects. The liturgical_reading would show lower extractiveness (coordination function without nationalist extraction). The native_daily_reading would show lower extractiveness (pure coordination of speaker communities without legitimacy layer). These are not three perspectives on one constraint — they are three constraints on one kernel. Link them via affects_constraints, not via multiple perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(continuity_narrative_reading, analytical, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
