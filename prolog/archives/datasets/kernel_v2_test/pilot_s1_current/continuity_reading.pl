% ============================================================================
% CONSTRAINT STORY: continuity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_continuity_reading, []).

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
 *   constraint_id: continuity_reading
 *   human_readable: Medieval Latin as Continuous Evolution (Continuity Reading)
 *   domain: historical_linguistics/philology/intellectual_history
 *
 * SUMMARY:
 *   The continuity reading treats Medieval Latin as Classical Latin after
 *   natural linguistic evolution, understanding medieval innovations as
 *   legitimate developments within a continuous tradition rather than as
 *   departures from classical norms. This reading instantiates one of three
 *   competing positions in the contested kernel 'correct_latin_kernel' — the
 *   framework through which scholars understand the relationship between
 *   classical and medieval Latin forms. The continuity reading has served
 *   powerful institutional interests (medieval ecclesiastical authority, the
 *   unified textual tradition) while simultaneously constraining alternative
 *   methodologies (discontinuity-based reconstruction) and suppressing
 *   acknowledgment of genuine medieval innovation. The constraint exhibits
 *   mixed coordination and extraction: genuine coordination exists (Medieval
 *   Latin as unified system enables comparative methods and textual
 *   analysis), but this coordination is bound up with institutional
 *   enforcement of a particular interpretive frame that benefits
 *   ecclesiastical authority and the continuist philological tradition. The
 *   theater ratio (0.58) has risen over the 900-year interval (medieval
 *   period to present), reflecting that contemporary academic practice
 *   maintains the continuity frame institutionally despite widespread
 *   scholarly acknowledgment of medieval innovations — the teaching frame and
 *   the research frame have diverged. Suppression, measured as the
 *   institutional pressure to interpret medieval forms as continuous
 *   development rather than acknowledging innovation, has declined from 0.50
 *   to 0.42 — humanist reforms, comparative reconstruction, and modern
 *   linguistics have made the alternative reading more defensible — but
 *   suppression persists because discontinuity reading remains costly for
 *   scholars whose authority depends on the continuity frame.
 *
 * KEY AGENTS:
 *   - Medieval Ecclesiastical Authority: Primary beneficiary (institutional/arbitrage) — derives linguistic authority from positioning as keeper of classical tradition; legitimizes church power through textual continuity
 *   - Medieval Scribe: Primary victim (powerless/trapped) — must justify all linguistic innovation as preservation of classical forms; cannot acknowledge their own language's living evolution
 *   - Comparative Philologist: Secondary victim (moderate/constrained) — benefits from continuity framing's explanatory power but constrained by institutional pressure to accept it as settled; cannot freely acknowledge reading as interpretive choice
 *   - Humanist Reform Movement: Organized challenger (organized/constrained) — seeks to distinguish classical from medieval forms; resists continuity narrative but pays cost in institutional standing
 *   - Post-Humanist Academic Consensus: Institutional maintainer (institutional/arbitrage) — preserves continuity reading through pedagogical practice and disciplinary convention despite acknowledged vulnerability
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — risks naturalizing contingent reading as immutable fact about language
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(continuity_reading, 0.35).
domain_priors:suppression_score(continuity_reading, 0.42).
domain_priors:theater_ratio(continuity_reading, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(continuity_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(continuity_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(continuity_reading, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(continuity_reading, tangled_rope).
narrative_ontology:human_readable(continuity_reading, "Medieval Latin as Continuous Evolution (Continuity Reading)").
narrative_ontology:topic_domain(continuity_reading, "historical_linguistics/philology/intellectual_history").

domain_priors:requires_active_enforcement(continuity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(continuity_reading, 'f012c041-88c1-43ff-8995-2629214e4639').
narrative_ontology:cs_kernel_codification('f012c041-88c1-43ff-8995-2629214e4639', fixed_text).
narrative_ontology:cs_authority_grounding('f012c041-88c1-43ff-8995-2629214e4639', lineage).
narrative_ontology:cs_interpretation_layer_present('f012c041-88c1-43ff-8995-2629214e4639').
narrative_ontology:cs_reading_relation('f012c041-88c1-43ff-8995-2629214e4639', continuity_reading__discontinuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('f012c041-88c1-43ff-8995-2629214e4639', continuity_reading__hybrid_reading, coexists_with).
narrative_ontology:cs_axiom('f012c041-88c1-43ff-8995-2629214e4639', foundational, medieval_development_continuity).
narrative_ontology:cs_axiom_status(medieval_development_continuity, holdable).
narrative_ontology:cs_axiom_grounding('f012c041-88c1-43ff-8995-2629214e4639', medieval_development_continuity, empirically_contingent).
narrative_ontology:cs_axiom('f012c041-88c1-43ff-8995-2629214e4639', secondary, ecclesiastical_textual_authority).
narrative_ontology:cs_axiom_status(ecclesiastical_textual_authority, overridden).
narrative_ontology:cs_axiom_grounding('f012c041-88c1-43ff-8995-2629214e4639', ecclesiastical_textual_authority, instrumental).
narrative_ontology:cs_reference_frame('f012c041-88c1-43ff-8995-2629214e4639', classical_textual_tradition).
narrative_ontology:cs_drift_state('f012c041-88c1-43ff-8995-2629214e4639', contemporary_historical_linguistics, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('f012c041-88c1-43ff-8995-2629214e4639', '').
narrative_ontology:cs_kernel_id(continuity_reading, correct_latin_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(continuity_reading, medieval_ecclesiastical_authority).
narrative_ontology:constraint_beneficiary(continuity_reading, continuist_philological_tradition).
narrative_ontology:constraint_victim(continuity_reading, classical_purity_claims).
narrative_ontology:constraint_victim(continuity_reading, reconstruction_methodology).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(continuity_reading, comparative_philologists).
narrative_ontology:constraint_victim(continuity_reading, medieval_scribes_copyists).
narrative_ontology:constraint_victim(continuity_reading, comparative_philologists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The medieval church sets and maintains the continuity reading as the authoritative framework for understanding Latin. Church authority depends on demonstrating that medieval Latin preserves the classical tradition — this positioning legitimizes the church as the keeper of authentic knowledge and the arbiter of correct Latin usage. The church can exit this frame by shifting legitimacy to other sources (scriptural authority, doctrinal coherence) if needed.
narrative_ontology:constraint_stakeholder(continuity_reading, medieval_church_hierarchy, agenda_setter,
    institutional, generational, arbitrage, continental).

% Medieval scribes must justify their linguistic innovations (new words, grammatical forms, stylistic choices) as preservation or correction of classical precedent. They cannot acknowledge genuine innovation without undermining the authority claim that legitimizes their textual work. They are trapped — exit would require abandoning the entire framework through which their scribal authority is understood.
narrative_ontology:constraint_stakeholder(continuity_reading, medieval_scribes_copyists, payer,
    powerless, biographical, trapped, local).

% Comparative philologists benefit from treating medieval Latin as a unified system — the continuity frame enables comparative reconstruction and cross-textual analysis. But they also bear costs: they face institutional pressure to accept the continuity reading as settled rather than acknowledging it as an interpretive choice. Career advancement depends on working within the continuity frame; challenging it invites criticism as 'lacking philological rigor.' Exit is possible but costly.
narrative_ontology:constraint_stakeholder(continuity_reading, comparative_philologists, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(continuity_reading, comparative_philologists, payer).

% Humanist reformers are partially excluded from the medieval continuity frame — their position (medieval forms are distinct innovations, not classical developments) contradicts the continuity reading's core claim. They challenge the frame but face institutional resistance from ecclesiastical authority and established philological practice. They would object to the continuity reading if given voice, but their objections are systematically devalued as 'purist' or 'prescriptive' rather than recognized as equally valid readings of the textual evidence.
narrative_ontology:constraint_stakeholder(continuity_reading, humanist_reformers, excluded,
    organized, generational, constrained, continental).

% The doctrine that 'classical purity is the standard for linguistic correctness' is a proposition, not an agent, but it is suppressed by the continuity reading. The reading treats medieval forms as legitimate developments rather than acknowledging them as departures from classical norms — this undermines the very basis for a purity standard. The constraint extracts from the potential for alternative methodologies that might ground themselves in classical standards.
narrative_ontology:constraint_stakeholder(continuity_reading, classical_purity_claims, payer,
    powerless, civilizational, trapped, universal).
narrative_ontology:stakeholder_non_agent(continuity_reading, classical_purity_claims).

% The doctrine that 'linguistic forms can be reconstructed by identifying patterns of change' is constrained by the continuity reading. If medieval forms must be understood as continuous developments rather than as evidence of systematic change, the methodological toolkit for reconstruction becomes limited. Alternative methodologies (discontinuity-based reconstruction, treating medieval as a separate system) are methodologically defensible but institutionally costly to pursue.
narrative_ontology:constraint_stakeholder(continuity_reading, reconstruction_methodology, payer,
    powerless, civilizational, constrained, global).
narrative_ontology:stakeholder_non_agent(continuity_reading, reconstruction_methodology).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The continuity reading solves a genuine coordination problem: How can medieval church authority maintain textual legitimacy while introducing innovations? Answer: by framing innovations as internal corrections or developments of classical forms, creating a unified framework where medieval and classical Latin are continuous. This coordination enabled the church to function as an authoritative interpretive community without having to repeatedly defend each innovation.
% TRANSFER_FUNCTION: The constraint transfers interpretive authority: Medieval ecclesiastical figures claim authority over Latin forms by positioning themselves as preservers of classical tradition. The transfer moves from classical literary tradition (owned by no medieval institution) to medieval church (which claims custodianship). Authority flows toward the church; suppression of alternative readings flows away from challengers.
% ABSENT_VOICES: Excluded from the discussion: (1) Medieval laypeople whose actual linguistic practices contradicted the continuity frame. (2) Discontinuity scholars (humanist reformers, modern historical linguists) whose challenge to the frame was systematized later and remains methodologically defensible. (3) The abstract proposition of 'classical standards' which could ground an alternative reading. These voices would object that medieval innovations are genuine changes, not developments, and that treating them as continuity misrepresents the historical record.
% DISAPPEARANCE_RATIONALE: If the continuity reading disappeared (were abandoned by scholarship), medieval Latin would be reframed as a historically distinct system with its own rules, innovations, and developmental trajectories. Some institutional structures would rearrange (how medieval texts are taught, how authority claims over 'correct Latin' are grounded), but the actual linguistic facts would not change. The disappearance would be administrative/institutional rather than worldmaking — the texts would remain, but their interpretation would shift. Whether this counts as the 'world rearranging' depends on whether you prioritize institutional frameworks (would rearrange) or brute linguistic facts (would not).
% FOUNDING_PROBLEM: The founding problem was ecclesiastical authority: How can the medieval church claim authority over written Latin while introducing innovations that depart from classical models? The continuity reading solved this by reframing innovations as legitimate developments, preserving both change and authority within a single framework.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem was live during the medieval period (ecclesiastical authority genuinely required this legitimacy claim). Modern secular scholarship confirms this historical analysis — historians and philologists agree that the church's institutional stability depended on textual authority claims. However, the founding problem is now DEAD: modern ecclesiastical authority no longer depends on demonstrating Latin continuity (it grounds itself in other sources), and modern secular scholarship has no stake in the church's authority. The problem the reading solved has dissolved — the reading persists through institutional inertia, not because the founding mandate is live. This is corroborated by the rise in theater_ratio (0.42 → 0.58) and the acknowledged scholarly skepticism of the continuity frame despite institutional persistence of it.
narrative_ontology:disappearance_verdict(continuity_reading, contested).
narrative_ontology:founding_problem_status(continuity_reading, dead).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL SCRIBE (SNARE) — Trapped within the constraint that medieval linguistic innovation must be justified as preservation of classical forms. Cannot exit: the scribe's authority depends on demonstrating continuity with classical precedent, not on acknowledging genuine innovation. Bears full cost of the reading's operational logic: must suppress acknowledgment of their own language's living evolution.
constraint_indexing:constraint_classification(continuity_reading, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: COMPARATIVE PHILOLOGIST (TANGLED ROPE) — Constrained by institutional pressure to accept the continuity narrative (required for legitimacy in medieval studies), but also benefits from its explanatory power (coherence across centuries, textual production). Experiences genuine coordination (Latin as unified system enables comparative method) alongside extraction (cannot acknowledge the reading as interpretive choice without losing standing).
constraint_indexing:constraint_classification(continuity_reading, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: MEDIEVAL ECCLESIASTICAL AUTHORITY (ROPE) — Benefits from the continuity reading: it legitimizes medieval church authority as the keeper of authentic classical tradition. The church's linguistic authority derives from this positioning. Low effective extraction — the reading genuinely coordinates church self-understanding with textual practices. Arbitrage option: can shift to alternative legitimacy claims (scripture, doctrine) if needed.
constraint_indexing:constraint_classification(continuity_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: HUMANIST REFORM MOVEMENT (TANGLED ROPE) — Organized challenge to the continuity reading; seeks to distinguish classical from medieval forms. Constrained by institutional resistance (church, university structures defend continuity narrative). Benefits from methodological innovations (comparative reconstruction) but bears extraction cost: must frame their position as 'correction' or 'purism' rather than as equally legitimate alternative reading.
constraint_indexing:constraint_classification(continuity_reading, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: POST-HUMANIST ACADEMIC CONSENSUS (PITON) — The continuity reading persists institutionally despite acknowledged vulnerability. Scholars teach the continuity framing in medieval Latin courses while privately acknowledging medieval innovations. Theater ratio (0.58) reflects this: the institutional performance of unified 'Latin' coexists with scholarly knowledge of historical discontinuity. The reading is maintained through pedagogical inertia and disciplinary convention rather than active epistemological commitment.
constraint_indexing:constraint_classification(continuity_reading, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational perspective, Latin continuity might appear as natural linguistic fact: languages evolve continuously, medieval forms develop from classical roots. However, this perspective risks naturalizing what is actually a contested interpretive choice — a reading chosen to serve specific institutional interests (church authority, textual legitimacy). The false-summit detector will flag this: the constraint's beneficiary structure (ecclesiastical authority, continuist tradition) contradicts the mountain classification.
constraint_indexing:constraint_classification(continuity_reading, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(continuity_reading_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(continuity_reading, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(continuity_reading, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(continuity_reading, TR),
    TR >= 0.70.

:- end_tests(continuity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The continuity reading generates extraction through institutional enforcement (medieval church benefits; scribes constrained), but the extraction is not extreme because the reading also provides genuine coordination benefits (unified textual system, comparative methods). The extraction is concentrated in beneficiary capture of legitimacy rather than in direct resource transfer. The measure reflects that medieval ecclesiastical authority captures authority claims through the reading without necessarily extracting material goods. Suppression (0.42): Moderate. Institutional pressure exists to maintain the continuity frame (career risk for scholars who adopt discontinuity reading; textual authority is staked on continuity). But suppression has declined as humanist and modern linguistics made alternatives increasingly defensible. Contemporary suppression is now mostly institutional convention rather than active coercion. Theater ratio (0.58): Moderate-high. The continuity reading is substantially performative in contemporary academic practice — scholars teach and publish in the continuity frame while privately acknowledging medieval innovations in specialist research. The constraint's function (legitimizing medieval textual authority) has partially atrophied in modern secular scholarship, leaving the institutional performance largely theatrical.
 *
 * PERSPECTIVAL GAP:
 *   The continuity reading shows maximum perspectival divergence between beneficiary and victim perspectives. Medieval ecclesiastical authority (Rope) experiences the reading as genuine coordination that legitimizes their textual practices. The medieval scribe (Snare) experiences the same reading as extraction — they must suppress acknowledgment of innovation to maintain their authority. The comparative philologist (Tangled Rope) experiences both: genuine methodological benefits from treating Latin as unified system, alongside extraction from institutional pressure to accept continuity as settled. The humanist reformer (Tangled Rope) experiences the reading's opposite — as suppression of their innovation reading. The post-humanist consensus (Piton) experiences institutional performance: the reading persists not because it is functionally central but because changing it would require institutional work. The analytical observer (Mountain candidate) risks naturalization: treating the continuity reading as an immutable feature of how language works, rather than as a contestable institutional choice.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values emerge from beneficiary/victim relationships and exit options. Medieval ecclesiastical authority has high institutional power and arbitrage-level exit (can ground authority in alternative sources if needed) — derives low/negative d → receives subsidy from the constraint. The medieval scribe has low power and trapped exit (authority depends entirely on continuity frame) — derives high d → experiences maximum extraction. The comparative philologist has moderate power and constrained exit (career cost to abandon continuity but methodologically defensible to do so) — derives moderate d → experiences moderate extraction. The humanist reformer has organized power and constrained exit (institutional resistance but growing alternative tradition) — derives moderate d → experiences extraction balanced against methodological agency. The post-humanist consensus has institutional power and arbitrage exit (can shift institutional frames if faced with sufficient challenge) — derives low d → experiences subsidy from conventional practice. The analytical observer operates outside the extraction flow entirely.
 *
 * MANDATROPHY ANALYSIS:
 *   The continuity reading's mandate was to legitimize medieval textual authority — to explain how medieval Latin could be simultaneously continuous with classical tradition AND the authoritative vehicle of church doctrine. This mandate was live during the medieval period (ecclesiastical authority genuinely depended on demonstrating textual continuity). The mandate has partially outlived its function: modern secular scholarship no longer depends on medieval ecclesiastical authority, and modern linguistics can explain medieval Latin through descent and contact rather than through continuity claims. The reading persists not because the original mandate is live but through institutional inertia (textbooks teach it, scholars learn it, career incentives maintain it). Theater ratio rising from 0.42 to 0.58 indicates growing mandatrophy: the reading's functional role has atrophied while institutional performance of the reading has intensified. This is diagnostic of piton classification from certain perspectives — the reading is maintained partly because abandoning it would require institutional coordination (curriculum revision, disciplinary reframing), not because its justification is live.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    continuity_vs_innovation_threshold,
    'At what degree of morphological, syntactic, or phonological change does a medieval Latin form cease to be a ''natural development'' of classical forms and become genuinely innovative?',
    'Systematic morphological comparison (ablative case erosion, synthetic vs. analytic constructions, vowel quality changes) across documented textual record; identification of change rates incompatible with continuous evolution from attested classical usage.',
    'If threshold is crossed by medieval innovations: continuity reading collapses; discontinuity reading becomes structurally more plausible. If threshold never reached: continuity reading gains empirical support. If threshold is indeterminate (continuous spectrum): hybrid reading becomes more defensible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(continuity_vs_innovation_threshold, empirical, 'Threshold distinguishing natural development from genuine innovation').

omega_variable(
    reconstruction_methodology_neutrality,
    'Does the continuity reading''s claim that medieval forms are ''internal corrections'' rest on neutral linguistic methodology, or does it embed a normative choice about what counts as legitimate change?',
    'Comparative analysis: apply discontinuity-reading''s reconstruction methodology to the same textual corpus; compare success rates and explanatory power. Identify whether continuity reading''s success depends on method choice.',
    'If neutral: continuity is genuinely more parsimonious. If method-dependent: the reading''s apparent empirical superiority is an artifact of the analytic framework; both readings become equally defensible at the methodological level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reconstruction_methodology_neutrality, conceptual, 'Whether continuity reading''s success depends on methodological choice').

omega_variable(
    ecclesiastical_authority_dependence,
    'To what degree does the institutional persistence of the continuity reading depend on its alignment with medieval church authority claims, rather than on its descriptive accuracy?',
    'Historical analysis: trace adoption of continuity reading through ecclesiastical institutions vs. secular philological centers; identify whether reading weakens in contexts where church authority is not at stake; examine how reading changes in post-Reformation philology.',
    'High dependence: the reading is structurally an instance of authority legitimacy maintenance (snare/tangled-rope from powerless perspectives). Low dependence: reading''s persistence indicates genuine explanatory power. Detected dependence upgrades victim classification of ''classical_purity_claims'' and ''reconstruction_methodology''.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ecclesiastical_authority_dependence, empirical, 'Degree to which reading''s persistence depends on ecclesiastical authority alignment').

omega_variable(
    sibling_reading_foreclosure,
    'Does the continuity reading genuinely foreclose the discontinuity reading, or do both readings remain coherent positions that differ only in normative framing?',
    'Logical analysis: identify whether continuity reading''s core axioms (Medieval-as-continuous-development) directly contradict discontinuity''s core axioms (Medieval-as-structural-break). If both readings can accommodate the same empirical data by different interpretive choices, they coexist; if one reading''s acceptance entails the other''s falsity, foreclosure applies.',
    'Foreclosure: this reading and discontinuity are mutually exclusive; only one can be true. Coexistence: both readings remain live for different parties; the constraint''s tension is permanent, not resolvable. Coexistence implies the constraint should be reclassified from tangled-rope (mixed extraction) to permanent institutional coexistence (upgraded to snare for both sibling readings).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'Whether continuity reading logically forecloses discontinuity reading').

omega_variable(
    natural_law_ambiguity,
    'Is the continuity reading a description of how language naturally works (mountain: continuous evolution is inherent to linguistic change), or is it a normative claim about how medieval scholars should have understood their own language (tangled rope: institutional enforcement of a particular interpretive frame)?',
    'Examination of medieval metalinguistic consciousness: do medieval grammarians, scribes, and scholars themselves claim to be preserving classical forms (supporting continuity narrative), or do they acknowledge innovation (contradicting natural-law framing)? What framework did medieval actors use to understand their own linguistic practices?',
    'If medieval consciousness supported continuity: reading may be natural law (mountain reclassification). If medieval consciousness acknowledged innovation: reading is normative imposition (tangled-rope confirmed; victims upgraded). If medieval consciousness is mixed/unclear: reading is interpretive imposition on ambiguous evidence (snare classification for reconstruction methodology).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_ambiguity, empirical, 'Whether continuity is natural law or normative institutional framing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(continuity_reading, 0, 900).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cont_tr_t0, continuity_reading, theater_ratio, 0, 0.42).
narrative_ontology:measurement(cont_tr_t300, continuity_reading, theater_ratio, 300, 0.48).
narrative_ontology:measurement(cont_tr_t600, continuity_reading, theater_ratio, 600, 0.58).
narrative_ontology:measurement(cont_tr_t900, continuity_reading, theater_ratio, 900, 0.58).

% Extraction over time
narrative_ontology:measurement(cont_be_t0, continuity_reading, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(cont_be_t300, continuity_reading, base_extractiveness, 300, 0.32).
narrative_ontology:measurement(cont_be_t600, continuity_reading, base_extractiveness, 600, 0.35).
narrative_ontology:measurement(cont_be_t900, continuity_reading, base_extractiveness, 900, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(cont_su_t0, continuity_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(cont_su_t300, continuity_reading, suppression_requirement, 300, 0.45).
narrative_ontology:measurement(cont_su_t600, continuity_reading, suppression_requirement, 600, 0.42).
narrative_ontology:measurement(cont_su_t900, continuity_reading, suppression_requirement, 900, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(continuity_reading, identity_coordination).
narrative_ontology:affects_constraint(continuity_reading, discontinuity_reading).
narrative_ontology:affects_constraint(continuity_reading, hybrid_reading).
narrative_ontology:affects_constraint(continuity_reading, humanist_textual_purity).
narrative_ontology:affects_constraint(continuity_reading, ecclesiastical_authority_legitimacy).

% DUAL FORMULATION NOTE:
% The continuity reading decomposes from the unified kernel 'correct_latin_kernel' into three distinct constraint stories (continuity_reading, discontinuity_reading, hybrid_reading). Each reading has its own ε value, beneficiary structure, and classification. The three readings form a constraint family linked by network.affects_constraints. The ε-invariance principle applies: what appears to be one constraint (the reading of Latin origins) is actually three constraints with different structural properties — they would show different extractiveness, suppression, and enforcement patterns. Decomposition is necessary because the choice of reading materially affects how extraction is distributed (ecclesiastical authority in continuity reading; humanist reformers in discontinuity reading; balanced recognition in hybrid reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(continuity_reading, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
