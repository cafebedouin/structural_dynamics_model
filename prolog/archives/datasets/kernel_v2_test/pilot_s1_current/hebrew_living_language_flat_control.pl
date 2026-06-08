% ============================================================================
% CONSTRAINT STORY: hebrew_living_language_flat_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_living_language_flat_control, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:flat_control_of/2,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_living_language_flat_control
 *   human_readable: Hebrew as a Living Language: Liturgical Recitation vs. Vernacular Generation
 *   domain: sociolinguistics/language_revitalization/commitment_systems
 *
 * SUMMARY:
 *   The commitment 'Hebrew is a living language' stabilized across two
 *   millennia with contested occupancy regarding what counts as 'living' —
 *   whether liturgical recitation alone sustains the status or whether native
 *   vernacular generation is required. This is a foundational constraint in
 *   sociolinguistics and language revitalization with deep entanglement in
 *   religious authority, state nation-building, and linguistic theory. The
 *   constraint exhibits a tangled rope structure: genuine coordination
 *   function (maintaining diaspora linguistic continuity across centuries of
 *   dispersion) layered with substantial extraction (control over who counts
 *   as a legitimate speaker, gatekeeping of linguistic authenticity,
 *   suppression of vernacular innovation as 'impure'). The measurement
 *   trajectory shows rising theater ratio (from 0.45 to 0.61 over 300 years)
 *   indicating increasing performative content as the founding coordination
 *   problem (diaspora continuity) became less urgent and institutional
 *   gatekeeping became more theatrical. Extractiveness declined slightly
 *   (0.65 → 0.52) during early revitalization periods when vernacular
 *   speakers captured institutional authority, then stabilized after state
 *   standardization reasserted top-down institutional form. The constraint
 *   operates at the intersection of three institutional actors: religious
 *   establishment (defining liturgical authority), secular revitalization
 *   movement (claiming the right to generate living vernacular), and state
 *   apparatus (standardizing Hebrew for educational and administrative
 *   purposes). The definitional boundary between 'liturgically sufficient'
 *   and 'requires vernacular generation' is not a linguistic discovery but a
 *   site of institutional power: who gets to decide what counts as authentic
 *   Hebrew, and what flows from that gatekeeping authority.
 *
 * KEY AGENTS:
 *   - Religious Establishment: Primary beneficiary (institutional/arbitrage) — defines and guards 'living language' status; maintains institutional legitimacy through control of canonical form
 *   - Vernacular Speakers: Primary victim (powerless/trapped) — their native speech practices are systematically devalued as 'not authentic' when they deviate from canonical form; exit means abandoning linguistic identity
 *   - Language Revitalization Movement: Secondary victim (moderate/constrained) — aims to generate living Hebrew vernacular but navigates religious establishment's delegitimization of non-canonical forms
 *   - State Actor (Israel/nation-state): Institutional beneficiary (institutional/mobile) — benefits from 'living Hebrew' status for nation-building; also imposes standardization that constrains revitalization creativity
 *   - Linguistic Academic Establishment: Theatrical arbiter (organized/constrained) — claims neutral 'objective' definition of 'living language' while enforcing a definition historically favored by religious authority; maintains scholarly apparatus that naturalizes the commitment
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing a contested commitment as a linguistic fact rather than an institutional arrangement
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_living_language_flat_control, 0.52).
domain_priors:suppression_score(hebrew_living_language_flat_control, 0.38).
domain_priors:theater_ratio(hebrew_living_language_flat_control, 0.61).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, extractiveness, 0.52).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(hebrew_living_language_flat_control, theater_ratio, 0.61).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_living_language_flat_control, tangled_rope).
narrative_ontology:human_readable(hebrew_living_language_flat_control, "Hebrew as a Living Language: Liturgical Recitation vs. Vernacular Generation").
narrative_ontology:topic_domain(hebrew_living_language_flat_control, "sociolinguistics/language_revitalization/commitment_systems").

domain_priors:requires_active_enforcement(hebrew_living_language_flat_control).

% --- Construction-pair linkage (forced-flat control of a kernel) ---
narrative_ontology:flat_control_of(hebrew_living_language_flat_control, hebrew_living_language).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, religious_establishment).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, linguistic_continuity_doctrine).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, competing_revitalization_movements).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, vernacular_speakers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, state_apparatus).
narrative_ontology:constraint_beneficiary(hebrew_living_language_flat_control, diaspora_communities).
narrative_ontology:constraint_victim(hebrew_living_language_flat_control, revitalization_movement).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Custodian of liturgical Hebrew and canonical linguistic authority across two millennia. Controls institutional definition of 'living language' status. Maintains Hebrew's sacred status through liturgical practice, religious education, and institutional gatekeeping of linguistic legitimacy. Benefits from the constraint's operation: defines what counts as authentic Hebrew, perpetuates institutional role as linguistic arbiter, maintains diaspora community through shared sacred language.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, religious_establishment, agenda_setter,
    institutional, civilizational, arbitrage, global).

% Native speakers of Hebrew vernacular whose living speech is systematically devalued as 'not authentic' when it deviates from liturgical canonical form. Cannot exit without abandoning linguistic identity. Excluded from decision-making about what counts as 'living Hebrew' even though they are the primary speakers generating new utterances. Bear the cost of institutional delegitimization: their innovations are treated as corruption rather than natural language evolution.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, vernacular_speakers, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language_flat_control, vernacular_speakers, excluded).

% Activists, educators, linguists, and community members committed to generating living Hebrew vernacular for modern communication. Face institutional pressure from religious establishment that delegitimizes non-canonical forms as impure or inauthentic. Have partial agency (can build alternative institutional structures, education systems, media) but must navigate religious authority's counter-authority. High barriers to exit: choosing to revitalize Hebrew locks them into managing the liturgical/vernacular boundary.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, revitalization_movement, payer,
    moderate, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language_flat_control, revitalization_movement, agenda_setter).

% The modern nation-state (Israel, or historically any state adopting Hebrew as official language) uses 'living Hebrew' status for nation-building legitimacy, shared national identity, and institutional cohesion. Imposes state-standardized form through educational institutions, media, and bureaucracy. Coordinates genuine linguistic commons for state administration and national communication. Also constrains vernacular creativity through standardization and top-down institutional control. Can exit by adopting different official language, but benefits from using Hebrew's historical legitimacy.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, state_apparatus, agenda_setter,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(hebrew_living_language_flat_control, state_apparatus, beneficiary).

% Scholarly discipline that provides ostensibly neutral definition of 'living language' (continuous native speaker base generating novel utterances). Maintains authority through professional apparatus: peer review, conferences, published standards. The definition appears objective but historically tracks the religious establishment's privileging of canonical form. Acts as institutional referee in the vernacular/liturgical contest. Constrained: professional incentives reward maintaining 'scientific objectivity' rather than acknowledging the commitment's institutional contestation.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, academic_linguistics_establishment, agenda_setter,
    organized, generational, constrained, global).

% Jewish diaspora communities that maintained Hebrew continuity through liturgical practice, prayer, and religious education for two millennia. Benefited from the constraint's coordination function: shared sacred language enabled community continuity across separation and linguistic pressure from dominant languages. Current status mixed: state sovereignty and secular institutions now provide much of the institutional support for Hebrew continuity; religious gatekeeping is less necessary for diaspora cohesion but remains institutionally powerful.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, diaspora_communities, beneficiary,
    moderate, generational, constrained, global).

% The doctrine that language authenticity maps to historical continuity and correct form. Not an agent but a vindicated institutional principle — the constraint's operation vindicates this doctrine by treating canonical forms as more authentic than living vernacular. The doctrine is neither beneficiary nor victim but a consequence of the constraint's institutional structuring.
narrative_ontology:constraint_stakeholder(hebrew_living_language_flat_control, linguistic_authenticity_doctrine, observer,
    powerless, civilizational, analytical, universal).
narrative_ontology:stakeholder_non_agent(hebrew_living_language_flat_control, linguistic_authenticity_doctrine).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintaining linguistic continuity for a diaspora separated from territorial state and institutional infrastructure, enabling communities scattered across the Mediterranean, Middle East, and Europe to participate in shared sacred language practice and maintain community identity across centuries of linguistic assimilation pressure.
% TRANSFER_FUNCTION: Linguistic legitimacy and institutional authority flow from vernacular speakers upward to the religious establishment that controls canonical form and gatekeeping of authenticity. The establishment transfers back institutional support for language maintenance (education, religious texts, liturgical framework) but on terms that require conformity to canonical form. Novel utterances are transferred from vernacular speakers to the archive of 'acceptable innovation' controlled by the establishment.
% ABSENT_VOICES: Secular linguistic innovators excluded from early periods (medieval through pre-modern era) when religious establishment held exclusive institutional authority. Women excluded from formal religious training where much Hebrew scholarship occurred. Non-elite speakers excluded from institutions defining legitimacy. Competing language communities (Yiddish speakers, Arabic-speaking Jews, Ladino speakers) excluded from conversations about Hebrew's living status despite their linguistic intimacy with Hebrew. In contemporary period: linguists and speakers from revitalization movements partially inside but with delegitimized voice.
% DISAPPEARANCE_RATIONALE: Religious establishment and liturgical scholars argue that if the constraint disappeared, Hebrew would cease to exist as a continuous tradition — the diaspora needed the commitment's coordination function to maintain linguistic continuity. Revitalization advocates argue that if the constraint disappeared, Hebrew would flourish MORE — vernacular speakers would generate living language unconstrained by gatekeeping of authenticity. State apparatus argues Hebrew is now independent of the constraint due to native speaker base and institutional infrastructure, but benefits from the commitment's historical legitimacy. The verdict reflects genuine structural contestation: the constraint enabled the very native speaker base that now makes it partially obsolete.
% FOUNDING_PROBLEM: After the dispersal of Jewish communities from territorial state (70 CE onward, crystallized after Bar Kokhba rebellion 132 CE), a diaspora separated by geography, dominant languages, and institutional fragmentation needed a mechanism to maintain shared linguistic and cultural identity. Hebrew had ceased to be a primary vernacular language and existed mainly in religious texts and prayer. The coordination problem: how to maintain linguistic continuity and community cohesion without territorial state, shared education system, or daily institutional interaction?
% FOUNDING_PROBLEM_CORROBORATION: State sovereignty (1948 CE) and Hebrew-medium education system (established 1950s-1960s) solved the original problem. Native speaker base now exists without dependence on religious institutional gatekeeping. Contemporary Hebrew maintenance is guaranteed by state institutions (education, media, bureaucracy), not by the commitment's coordination mechanism. The founding problem is acknowledged as historical fact by linguists, historians, and even contemporary religious establishment — the establishment explicitly frames its role as historical preservation rather than contemporary necessity for language survival.
narrative_ontology:disappearance_verdict(hebrew_living_language_flat_control, contested).
narrative_ontology:founding_problem_status(hebrew_living_language_flat_control, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: VERNACULAR SPEAKER (SNARE) — Native speakers of Hebrew vernacular find their linguistic practice systematically devalued as 'not authentic' when it deviates from liturgical form. Trapped: exit means abandoning native linguistic identity. The constraint extracts linguistic legitimacy from living speakers and transfers it to the authority that guards canonical form. Maximum extraction experienced because the speaker's own language is deemed inauthentic by institutional arbiters.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: LANGUAGE REVITALIZATION MOVEMENT (TANGLED ROPE) — Actors invested in Hebrew vernacular generation experience mixed coordination and extraction. Real coordination function: creating a shared linguistic commons for a diaspora or emerging nation-state. Real extraction: the liturgical establishment gatekeeps authority to define 'living language' status, constraining what counts as legitimate revitalization. Constrained exit: can build alternative institutional structures but must navigate religious authority's delegitimization.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: RELIGIOUS ESTABLISHMENT (ROPE) — Primary beneficiary. Experiences the constraint as coordination: liturgical recitation defines a shared sacred language that binds diaspora communities across two millennia. The establishment can arbitrage between liturgical authority (canonical form) and vernacular pressure (tolerating innovation while maintaining canonical primacy). Net beneficiary: legitimacy and institutional continuity flow to the establishment; their definition of 'living' is the authoritative one.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: LINGUISTIC ACADEMIC ESTABLISHMENT (PITON) — Nominally neutral arbiters of 'living language' status, but the academic field's definition (continuous native speaker base generating novel utterances) is theatrically sustained through a scholarly apparatus that claims objectivity while enforcing a definition that historically favored liturgical authority. Theater ratio reflects performative certainty about linguistic categories that are fundamentally contestable. The academic discipline maintains the 'neutral arbiter' role through ritualized methodology rather than through genuinely independent verification.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, piton,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: STATE ACTOR (TANGLED ROPE) — The modern state (Israel, or historically any nation-state adopting Hebrew) benefits from Hebrew's 'living' status: it legitimizes the state apparatus, provides a shared official language, and enables nation-building. Real coordination: creating a functional national linguistic commons. Real extraction: the state's definition of 'living Hebrew' (standardized, state-regulated education and media) privileges state institutional forms over vernacular creativity. Mobile exit: the state can redefine what counts as official language, but choosing to revitalize Hebrew specifically locks it into managing the liturgical/vernacular boundary.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, tangled_rope,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURALIZATION VIEW (MOUNTAIN) — From a civilizational vantage, dead languages are defined by absence of native speakers and discontinuous transmission; the definition of 'living language' as requiring vernacular generation appears as a natural linguistic category, not a constructed commitment. The liturgical-only model appears to violate this natural category. However, this perspective risks naturalizing what is actually a historical and institutional fact: that the liturgical/vernacular boundary became the arbiter of 'living' status precisely because religious institutional authority defined it that way, not because linguistics discovered it.
constraint_indexing:constraint_classification(hebrew_living_language_flat_control, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_living_language_flat_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(hebrew_living_language_flat_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hebrew_living_language_flat_control, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_living_language_flat_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(hebrew_living_language_flat_control, TR),
    TR >= 0.70.

:- end_tests(hebrew_living_language_flat_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-to-high. The constraint extracts linguistic legitimacy from vernacular speakers and transfers it upward to the religious establishment. The primary extraction mechanism is definitional: the establishment defines 'living language' in a way that privileges its own institutional role (canonical form guardian) over vernacular generation. This is not simple coercion (the vernacular speakers are not physically prevented from speaking) but institutional authority gatekeeping. The value reflects genuine coordination function (diaspora continuity) alongside substantial extraction. Suppression (0.38): Moderate. Significant barriers to vernacular innovation: social prestige attached to canonical forms, institutional pathways that privilege liturgical knowledge, educational systems emphasizing correct form. But suppression is not maximal because vernacular speakers DO generate Hebrew throughout the historical period — the suppression is more about institutional delegitimization than prohibition. Theater ratio (0.61): Moderate-to-high. Increasing over the interval. The linguistic academic establishment maintains performative certainty about what 'living language' means (requiring continuous native speaker base generating novel utterances) while historically this same establishment accepted liturgical-only preservation when religious authority defined it. The theater reflects that the 'neutral arbiter' role is itself performed — the definition is treated as objective when it is actually historically contingent on institutional power dynamics. Theater rose as the founding coordination problem became less urgent (state sovereignty achieved, native speaker base established 1950s onward) and the constraint became more about maintaining institutional authority than solving genuine coordination problems.
 *
 * PERSPECTIVAL GAP:
 *   The vernacular speaker (powerless/trapped) experiences snare: their own language is deemed inauthentic by institutional authorities. The religious establishment (institutional/arbitrage) experiences rope: they see the constraint as genuine coordination, creating a shared sacred language across diaspora. The revitalization movement (moderate/constrained) experiences tangled rope: real coordination mixed with real extraction gatekeeping. The state actor (powerful/mobile) experiences tangled rope: benefits from nation-building through shared language but imposes standardization that constrains vernacular creativity. The academic establishment (organized/constrained) experiences piton: maintains a degraded but theatrically sustained 'objective' linguistic position. The analytical observer (analytical/analytical) risks experiencing mountain: naturalizing the definitional boundary as a law of linguistics rather than a site of institutional power. The perspectival gap reveals that no single classification is correct — the constraint IS genuinely coordinating (diaspora continuity) AND genuinely extractive (gatekeeping of authenticity). The gap between snare (victim's experience) and rope (beneficiary's experience) is the extraction mechanism itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) for each perspective flows from power level, exit options, and beneficiary/victim status. Vernacular speakers: powerless + trapped + victim status → high d (maximum experienced extraction). Religious establishment: institutional + arbitrage + beneficiary → low d (effective subsidy; constraint benefits them). Revitalization movement: moderate + constrained + victim/beneficiary mix → moderate d (some extraction, some benefit). State: powerful + mobile + beneficiary → moderate-to-low d (can arbitrage). Academic establishment: organized + constrained + beneficiary → low-to-moderate d (maintains institutional authority). Analytical observer: analytical context → analytical deriv special case (observes the structure without inhabiting it). The effective extraction (chi) experienced by each agent is computed from d through the sigmoid function: trapped agents with victim status experience near-maximum chi; arbitrage-exit beneficiaries experience near-minimum chi; constrained mixed actors experience moderate chi. The directionality gap between powerless victims and institutional beneficiaries is the perspectival foundation of the constraint's classification as tangled rope.
 *
 * MANDATROPHY ANALYSIS:
 *   FOUNDING PROBLEM: Maintaining linguistic continuity for a diaspora without territorial state or institutional infrastructure. ORIGINAL SOLUTION: Religious establishment as custodian of liturgical Hebrew, enabling diaspora communities to maintain shared sacred language across centuries of separation and linguistic pressure from dominant languages (Greek, Latin, Arabic, Yiddish, etc.). FOUNDING PROBLEM STATUS TIMELINE: Live for ~1,500 years (post-diaspora, post-Bar Kokhba ~132 CE through ~1950 CE). Begins to become vestigial after state sovereignty (1948 CE) and establishment of Hebrew-speaking population (1950s onward). By late 20th century, the original problem is substantially solved: native speaker base exists, state-mandated education maintains Hebrew, institutional infrastructure is no longer dependent on religious authority for continuity. CONTEMPORARY PUZZLE: The institutional gatekeeping persists beyond the coordination problem it solved. The religious establishment continues to enforce the 'living language' definition privileging canonical form, but this gatekeeping no longer serves the original coordination function — it now primarily serves the institutional authority's own perpetuation. MANDATROPHY SIGNAL: Theater ratio rising from 0.45 to 0.61 while extractiveness remains elevated (0.52) indicates the constraint has partially decayed into piton (performative institutional maintenance) while retaining tangled rope structure. The constraint is not fully mandatrophic (it still has coordination function for religious communities maintaining liturgical practice) but shows significant signs of mandate atrophy: the state and academic establishment provide the real institutional supports for Hebrew's 'living' status; the religious establishment's gatekeeping role is increasingly theatrical. RESOLUTION: Acknowledge that the constraint serves two distinct contemporary functions: (1) Genuine coordination for religious communities maintaining liturgical practice (legitimate rope/tangled rope for that constituency). (2) Institutional authority gatekeeping for the religious establishment's own perpetuation (extraction mechanism). These may require decomposition if we apply strict ε-invariance — one constraint for the liturgical coordination, another for the gatekeeping extraction. For the flat control story, the tangled rope classification appropriately captures both the genuine coordination and the substantial extraction present in the current structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    living_language_definition_contestation,
    'Is ''living language'' defined by continuous native speaker base with generative capacity, or can sustained liturgical recitation across a diaspora constitute ''living'' status even without vernacular generation?',
    'Historical examination of which speakers recognized Hebrew as ''theirs'' at different periods; cross-cultural comparison with other liturgically-sustained languages (Latin, Classical Arabic, Sanskrit); analysis of what institutional actors gained by privileging one definition over another.',
    'If vernacular generation is required: the constraint is substantially extractive (religious establishment enforces an unnatural restriction). If liturgical recitation suffices: the constraint is closer to rope (genuine coordination problem around diaspora continuity). The definitional boundary is the extraction mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(living_language_definition_contestation, conceptual, 'The contested definition of ''living language'' as a linguistic or institutional question').

omega_variable(
    institutional_gatekeeping_function,
    'Does the religious establishment''s role as arbiter of ''living language'' status serve a genuine coordination function (maintaining linguistic continuity across diaspora) or primarily an extraction function (controlling who counts as a legitimate speaker)?',
    'Historical analysis of institutional decisions: which innovations were permitted and which suppressed? What pattern emerges? Interview or textual evidence from religious authority explaining their gatekeeping criteria. Comparison with cases where gatekeeping was transferred to secular institutions (Israeli academia, state education ministry).',
    'If coordination-primary: the constraint is tangled rope with substantial genuine function. If extraction-primary: the constraint is closer to snare with coordination as a cover story. The answer determines whether the religious establishment''s institutional role is justified by its function or by its power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_gatekeeping_function, empirical, 'Whether religious gatekeeping serves genuine coordination or primarily extraction').

omega_variable(
    vernacular_generation_capacity,
    'At historical moments when Hebrew was liturgically preserved but not natively spoken (pre-20th century diaspora), did the constraint prevent vernacular generation, or did speakers generate vernacular Hebrew despite institutional pressure toward canonical form?',
    'Textual and linguistic analysis of medieval and early modern Hebrew documents: Yiddish loanwords, Judeo-Arabic texts, commercial records, personal correspondence. Did living generation occur? Was it suppressed or merely not elevated to institutional status? Evidence from communities where Hebrew was NOT maintained liturgically: did they generate Hebrew vernacular under different conditions?',
    'If vernacular generation was suppressed: constraint is extractive snare. If vernacular generation occurred but was delegitimized: constraint is tangled rope with institutional narrative power. If vernacular generation was absent entirely: the definition of ''living'' may have been genuinely constrained by material conditions, not institutional extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(vernacular_generation_capacity, empirical, 'Whether the constraint suppressed native Hebrew speech or merely failed to institutionalize it').

omega_variable(
    mandatrophy_founding_problem_status,
    'Was the founding problem of the ''living language'' commitment — maintaining a diaspora''s linguistic continuity — still live at different historical periods, or had it become a vestigial institutional maintenance?',
    'Timeline analysis: When was the constraint established (post-diaspora, post-Bar Kokhba ~132 CE)? When did the founding problem cease (national sovereignty 1948 CE? State-mandated education? Current day?). Does institutional gatekeeping persist beyond the problem it solved?',
    'If founding problem is dead but constraint persists: the story is either piton (theatrical maintenance) or an authority structure that has become extractive after ceasing to coordinate. If the founding problem remains live: the constraint may be justified tangled rope. The timing determines mandatrophy diagnosis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandatrophy_founding_problem_status, empirical, 'Whether the founding coordination problem of the ''living language'' commitment remains live or is vestigial').

omega_variable(
    multiple_constraint_family,
    'Is this a single constraint (''Hebrew is living'') or multiple structurally distinct constraints: (1) liturgical continuity across diaspora (coordination), (2) gatekeeping of authenticity (extraction), (3) state language standardization (institutional coordination), each with different ε values?',
    'Apply ε-invariance principle: measure the constraint under different observables. If liturgical sustainability yields high coordination value and low extraction, while standardization yields high extraction, the observables are measuring different constraints. Decompose if needed.',
    'If multiple constraints: write separate stories per the ε-invariance principle. If single constraint: the tangled rope classification is appropriate. The answer clarifies whether this story should decompose into a family or remain unified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(multiple_constraint_family, conceptual, 'Whether the ''living language'' commitment decomposes into multiple structurally distinct constraints').

omega_variable(
    contemporary_enforcement_mechanism,
    'What enforces the ''living language'' commitment today, and at what cost? Is enforcement active (religious/state institutions actively suppressing alternatives) or passive (institutional default being followed)?',
    'Current observation: What happens to Hebrew speakers who innovate beyond canonical form? Are they marginalized by institutions? By communities? By both? Is the enforcement subtle (social prestige attached to canonical form) or explicit (institutional correction)? How does enforcement differ across religious vs. secular communities?',
    'If enforcement is active and costly: the constraint remains substantially extractive. If enforcement is passive/social: the constraint may be degrading piton. The contemporary enforcement picture shows whether suppression (0.38) is actively maintained or inertial.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contemporary_enforcement_mechanism, empirical, 'Whether contemporary enforcement of the ''living language'' commitment is active or passive').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_living_language_flat_control, 0, 300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebrew_theater_diaspora_era, hebrew_living_language_flat_control, theater_ratio, 0, 0.45).
narrative_ontology:measurement(hebrew_theater_medieval_period, hebrew_living_language_flat_control, theater_ratio, 100, 0.55).
narrative_ontology:measurement(hebrew_theater_enlightenment_period, hebrew_living_language_flat_control, theater_ratio, 200, 0.62).
narrative_ontology:measurement(hebrew_theater_early_revitalization, hebrew_living_language_flat_control, theater_ratio, 250, 0.68).
narrative_ontology:measurement(hebrew_theater_state_standardization, hebrew_living_language_flat_control, theater_ratio, 280, 0.61).
narrative_ontology:measurement(hebrew_theater_contemporary, hebrew_living_language_flat_control, theater_ratio, 300, 0.61).

% Extraction over time
narrative_ontology:measurement(hebrew_extractiveness_diaspora_era, hebrew_living_language_flat_control, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(hebrew_extractiveness_medieval_period, hebrew_living_language_flat_control, base_extractiveness, 100, 0.58).
narrative_ontology:measurement(hebrew_extractiveness_enlightenment_period, hebrew_living_language_flat_control, base_extractiveness, 200, 0.52).
narrative_ontology:measurement(hebrew_extractiveness_early_revitalization, hebrew_living_language_flat_control, base_extractiveness, 250, 0.48).
narrative_ontology:measurement(hebrew_extractiveness_state_standardization, hebrew_living_language_flat_control, base_extractiveness, 280, 0.52).
narrative_ontology:measurement(hebrew_extractiveness_contemporary, hebrew_living_language_flat_control, base_extractiveness, 300, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(hebrew_suppression_diaspora_era, hebrew_living_language_flat_control, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(hebrew_suppression_medieval_period, hebrew_living_language_flat_control, suppression_requirement, 100, 0.42).
narrative_ontology:measurement(hebrew_suppression_enlightenment_period, hebrew_living_language_flat_control, suppression_requirement, 200, 0.35).
narrative_ontology:measurement(hebrew_suppression_early_revitalization, hebrew_living_language_flat_control, suppression_requirement, 250, 0.38).
narrative_ontology:measurement(hebrew_suppression_state_standardization, hebrew_living_language_flat_control, suppression_requirement, 280, 0.38).
narrative_ontology:measurement(hebrew_suppression_contemporary, hebrew_living_language_flat_control, suppression_requirement, 300, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_living_language_flat_control, identity_coordination).
narrative_ontology:boltzmann_floor_override(hebrew_living_language_flat_control, 0.12).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, yiddish_linguistic_suppression).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, hebrew_state_standardization_pedagogy).
narrative_ontology:affects_constraint(hebrew_living_language_flat_control, religious_authority_legitimacy_authority).

% DUAL FORMULATION NOTE:
% The 'living language' commitment interlocks with state standardization and religious authority legitimacy. If decomposed per ε-invariance, separate stories would model: (1) liturgical continuity coordination (rope-family), (2) vernacular generation gatekeeping (snare-family), (3) state language standardization (tangled rope with different beneficiary/victim structure). Current story is unified tangled rope; alternative decomposition would create constraint family with network edges. No decomposition performed in this flat control story.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(hebrew_living_language_flat_control, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
