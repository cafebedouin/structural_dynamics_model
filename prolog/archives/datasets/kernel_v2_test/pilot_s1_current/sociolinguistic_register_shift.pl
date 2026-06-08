% ============================================================================
% CONSTRAINT STORY: sociolinguistic_register_shift
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sociolinguistic_register_shift, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: sociolinguistic_register_shift
 *   human_readable: Humanist Classical Latin Reconstruction and Medieval Vernacular Displacement
 *   domain: historical_linguistics/intellectual_history
 *
 * SUMMARY:
 *   The Renaissance humanist reconstruction of Classical Latin from medieval
 *   vernacular forms constitutes a major sociolinguistic and intellectual
 *   constraint that operated across continental Europe from the 14th through
 *   17th centuries. The core tension is between two perspectives on 'correct
 *   Latin': medieval clerical Latin as a living, evolving written standard
 *   that had naturally drifted from ancient forms through centuries of use,
 *   adaptation, and regional variation; versus humanist Classical Latin as a
 *   recoverable ideal form grounded in the most authoritative ancient texts
 *   (Cicero, Livy, Virgil). This constraint exhibits the full range of
 *   Deferential Realism classifications. For the medieval clerical
 *   practitioners, the humanist revaluation appears as a snare — their
 *   professional competence is suddenly redefined as corrupt, requiring
 *   wholesale abandonment of training and reidentification. For cathedral
 *   schools and regional institutions, it is tangled rope — genuine
 *   coordination function (access to classical texts, integration with
 *   international networks) coupled with extraction (retraining costs,
 *   displacement). For the humanist elite, it is rope — solving a genuine
 *   epistemic problem (how to read ancient texts correctly). For printing and
 *   standardization movements, it is scaffold — a temporary coordination
 *   problem with built-in sunset as classical texts proliferate. For
 *   ecclesiastical institutions maintaining medieval forms, it becomes piton
 *   — classical correctness as official ideology, medieval forms in actual
 *   practice, the whole system increasingly theatrical. From the
 *   civilizational analytical perspective, it risks appearing as mountain —
 *   natural linguistic law — but the structural data of beneficiaries and
 *   victims triggers false summit detection. The constraint's theater ratio
 *   rises from 0.35 in 1400 (humanist standards nascent, coexisting with
 *   medieval practice) to 0.82 by 1500 (classical correctness becomes
 *   performative standard while medieval forms persist in marginal contexts).
 *   Extraction follows a similar arc, peaking around 1475 as humanist
 *   gatekeeping solidifies, then slightly declining by 1500 as the classical
 *   standard becomes more naturalized and less actively suppressed.
 *   Suppression requirement rises monotonically as enforcement infrastructure
 *   builds — papal Latin commissions, printing standardization, educational
 *   reform.
 *
 * KEY AGENTS:
 *   - Medieval Clerical Practitioners: Primary victims (powerless/trapped) — scribes, copyists, liturgical officials whose entire professional competence becomes 'corrupt' through external revaluation
 *   - Medieval Vernacular Written Tradition: Primary victim (powerless/trapped) — abstract epistemic commons; medieval texts become relegated to 'corrupt' status; the written record of medieval intelligence becomes delegitimized
 *   - Humanist Intellectual Elite: Primary beneficiaries (institutional/arbitrage) — capture prestige, patronage, institutional authority; monopolize interpretation of ancient texts; gain cosmopolitan mobility through mastery of standardized classical form
 *   - Provincial Cathedral Schools: Secondary victim (moderate/constrained) — face pressure to reorganize curriculum; must retrain faculty; lose institutional prestige if they resist
 *   - Printing Press Coalition: Organized actors (organized/constrained) — benefit from standardization (uniform classical orthography across printed books); actively enforce the standard through editorial practice
 *   - Ecclesiastical Latin Maintenance: Institutional actor (institutional/arbitrage) — initially resistant, eventually compromises (classical forms for official documents, medieval for practice); maintains hybrid system
 *   - Analytical Observer: Civilizational perspective (analytical/analytical) — at risk of naturalizing a contingent institutional power play as a natural law of linguistic evolution
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sociolinguistic_register_shift, 0.45).
domain_priors:suppression_score(sociolinguistic_register_shift, 0.5).
domain_priors:theater_ratio(sociolinguistic_register_shift, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sociolinguistic_register_shift, extractiveness, 0.45).
narrative_ontology:constraint_metric(sociolinguistic_register_shift, suppression_requirement, 0.5).
narrative_ontology:constraint_metric(sociolinguistic_register_shift, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sociolinguistic_register_shift, tangled_rope).
narrative_ontology:human_readable(sociolinguistic_register_shift, "Humanist Classical Latin Reconstruction and Medieval Vernacular Displacement").
narrative_ontology:topic_domain(sociolinguistic_register_shift, "historical_linguistics/intellectual_history").

domain_priors:requires_active_enforcement(sociolinguistic_register_shift).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sociolinguistic_register_shift, humanist_intellectual_elite).
narrative_ontology:constraint_beneficiary(sociolinguistic_register_shift, classical_literary_canon).
narrative_ontology:constraint_victim(sociolinguistic_register_shift, medieval_clerical_practitioners).
narrative_ontology:constraint_victim(sociolinguistic_register_shift, vernacular_written_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(sociolinguistic_register_shift, cathedral_schools_and_regional_institutions).
narrative_ontology:constraint_beneficiary(sociolinguistic_register_shift, printing_press_coalition).
narrative_ontology:constraint_beneficiary(sociolinguistic_register_shift, ecclesiastical_institution).
narrative_ontology:constraint_victim(sociolinguistic_register_shift, cathedral_schools_and_regional_institutions).
narrative_ontology:constraint_victim(sociolinguistic_register_shift, ecclesiastical_institution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Trained through decades of copyist and scrivener work in medieval Latin forms. Their professional competence is suddenly revalued as corrupt by humanist standards. Exit would require abandoning career identity and retraining in an alien form. They bear the full cost of the revaluation — loss of prestige, marginalization of their expertise, career barriers. No alternative legitimate practice pathway.
narrative_ontology:constraint_stakeholder(sociolinguistic_register_shift, medieval_clerical_practitioners, payer,
    powerless, biographical, trapped, continental).

% The entire corpus of medieval literature, theology, administration, and knowledge-recording becomes relegated to 'corrupt Latin.' Texts that were authoritative become dismissed as ignorant. The epistemic value of medieval intellectual production becomes degraded. The written record cannot organize to defend itself or migrate to alternative standards.
narrative_ontology:constraint_stakeholder(sociolinguistic_register_shift, medieval_written_epistemic_commons, payer,
    powerless, generational, trapped, continental).
narrative_ontology:stakeholder_non_agent(sociolinguistic_register_shift, medieval_written_epistemic_commons).

% Occupy university chairs, court positions, papal favor, and publication gatekeeping roles. Benefit from monopoly on interpretation of ancient texts; capture prestige and patronage through mastery of classical forms; achieve cosmopolitan mobility through standardized educated form. Can move between patrons and regions freely; control which forms count as legitimate. Net beneficiary from the constraint — they extract status and authority.
narrative_ontology:constraint_stakeholder(sociolinguistic_register_shift, humanist_intellectual_elite, beneficiary,
    institutional, immediate, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sociolinguistic_register_shift, humanist_intellectual_elite, agenda_setter).

% Face pressure to reorganize curricula around humanist standards. Some schools gain access to classical texts and international networks (benefit); all face retraining costs and loss of institutional prestige if they resist (cost). Can choose to adopt selectively or resist, but resistance carries penalty of marginalization. Mixed position with constrained agency.
narrative_ontology:constraint_stakeholder(sociolinguistic_register_shift, cathedral_schools_and_regional_institutions, payer,
    moderate, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(sociolinguistic_register_shift, cathedral_schools_and_regional_institutions, beneficiary).

% Printers, typesetters, editors, and standardization movements benefit from unified classical Latin orthography (eliminates regional variation costs, enables economies of scale in classical text production). Actively enforce the standard through editorial practice and type design. Face resistance from medieval-form clients; coordinate with humanist authorities on standards. Moderate agency — can choose which texts to print and which standards to enforce, but operate within constraints of patronage and market demand.
narrative_ontology:constraint_stakeholder(sociolinguistic_register_shift, printing_press_coalition, agenda_setter,
    organized, generational, constrained, continental).
narrative_ontology:stakeholder_secondary_role(sociolinguistic_register_shift, printing_press_coalition, beneficiary).

% Institutional church initially resists humanist standards, maintaining medieval Latin through institutional momentum. Gradually compromises (classical forms for official documents and prestige texts, medieval-influenced forms for actual liturgical and administrative practice). Eventually maintains hybrid system: classical correctness as official ideology, medieval practice as functional reality. High-level arbitrage optionality (can choose compromise position), but increasingly constrained by printing standardization and elite pressure.
narrative_ontology:constraint_stakeholder(sociolinguistic_register_shift, ecclesiastical_institution, payer,
    institutional, civilizational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(sociolinguistic_register_shift, ecclesiastical_institution, beneficiary).

% Observes the constraint from civilizational timescale and universal scope. Risks naturalizing the humanist victory as discovery of natural linguistic law (languages drift, ancients precede moderns, recovery requires textual sources). At risk of false summit: interpreting a contestable institutional power choice as natural inevitability. Neither pays nor benefits — observes structure from outside the constraint system.
narrative_ontology:constraint_stakeholder(sociolinguistic_register_shift, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: How to access and interpret ancient Latin texts correctly — the genuine epistemic problem humanists are solving. Medieval Latin readers cannot read Cicero and Virgil on their own terms because drift has obscured idiom, vocabulary, and grammar. Standardizing on classical forms enables the intellectual community to reconstruct and share understanding of ancient sources.
% TRANSFER_FUNCTION: The constraint moves intellectual legitimacy, prestige, and institutional authority from medieval practitioners and traditions to humanist elites. Medieval clerics lose status; humanists gain gatekeeping power over what counts as educated Latin. The transfer is asymmetric: humanists don't just gain prestige relative to medieval practitioners — medieval practitioners lose the prestige they previously held.
% ABSENT_VOICES: Merchants, craftspeople, and vernacular writers who never participated in the Latin conversation are absent. Their voices would note that classical Latin is irrelevant to their needs; they speak and write in vernacular. Medieval clergy who resist humanist pressure in regional contexts are present in some regions but excluded from the metropolitan humanist centers where the standard is being imposed. If asked, they would argue that medieval Latin is adequate for their purposes and that the humanist project is pretentious classicism, not legitimate recovery.
% DISAPPEARANCE_RATIONALE: If the constraint disappeared (humanists failed to impose classical standard, medieval forms continued as legitimate standard), the intellectual world would rearrange substantially. Access to ancient texts would remain mediated through medieval-trained scholars' interpretations. The humanist program of direct classical engagement would not exist. Educational institutions would continue training scribes in medieval forms. The international intellectual community would not coalesce around classical correctness as a shared standard. Prestige and patronage would flow differently — humanists would not have the gatekeeping power they actually acquired.
% FOUNDING_PROBLEM: The intellectual problem of how to read and interpret ancient Latin texts correctly when medieval practice has drifted substantially from ancient forms. Medieval Latin readers trained in contemporary practice cannot read Cicero on Cicero's own terms; idioms are opaque, vocabulary is unfamiliar, grammar seems incorrect. The constraint was built to solve this genuine epistemic problem: reconstruct classical forms so readers can understand ancient authors.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested in humanist writings (Petrarch's complaints about being unable to understand Livy correctly; Poliziano's philological arguments about textual corruption; Valla's demonstration of errors in medieval interpretations). However, medieval practitioners' own writings suggest they did not experience the problem as urgent — medieval commentaries on ancient texts offer interpretations without expressing that the difficulty is from linguistic drift. The humanist narrative of medieval incomprehension is partly true (medieval readings are often inaccurate by ancient standards) and partly rhetorical (medieval readers did not perceive their readings as failed until humanists revalued the standard). The founding problem is corroborated by textual evidence of misinterpretation, but the humanist account overstates medieval helplessness. Medieval readers had working interpretations; humanists offered more accurate ones and wrapped the improvement in naturalizing language about 'recovering' truth rather than 'choosing a different standard.'
narrative_ontology:disappearance_verdict(sociolinguistic_register_shift, world_rearranges).
narrative_ontology:founding_problem_status(sociolinguistic_register_shift, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: MEDIEVAL SCRIBE (SNARE) — Trained in medieval Latin forms through decades of practice; faces complete epistemic closure when humanist standards declare their Latin 'corrupt' and 'unlearned.' Exit requires abandoning professional identity, retraining in an alien form, and admitting lifetime of work was illegitimate. Structural suppression from elite redefinition of correctness; no alternative pathway to legitimate practice.
constraint_indexing:constraint_classification(sociolinguistic_register_shift, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: PROVINCIAL CATHEDRAL SCHOOL (TANGLED ROPE) — Must coordinate with emerging humanist standards to maintain epistemic legitimacy (genuine coordination function: access to classical texts, integration with international scholarly networks). Simultaneously bears extraction: curriculum reorganization, retraining costs, displacement of established teaching practices. Constrained exit — some schools adopt selectively, others face marginalization and loss of patronage.
constraint_indexing:constraint_classification(sociolinguistic_register_shift, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: HUMANIST INTELLECTUAL ELITE (ROPE) — Experiences the Latin reconstruction as pure coordination: recovering classical forms solves the legitimate epistemic problem of accessing ancient texts on their own terms. Benefits from social prestige, patronage concentration, and monopoly on correct interpretation. High exit optionality — can move between courts, regions, patrons freely. The constraint appears as intellectual progress and rightful restoration.
constraint_indexing:constraint_classification(sociolinguistic_register_shift, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: PRINTING PRESS AND STANDARDIZATION MOVEMENT (SCAFFOLD) — Organized printers and scholars coordinating on classical Latin orthography and grammar see the constraint as transitional: standardized Latin printing enables distribution of classical texts, which in turn makes medieval forms obsolete through sheer volume and accessibility of superior models. Sunset logic: as printed classical texts proliferate (16th-17th centuries), the old medieval standard dies naturally through market pressure, not through coercive enforcement. Beneficiaries from coordination; moderate extraction during transition; sunset is built into the mechanism.
constraint_indexing:constraint_classification(sociolinguistic_register_shift, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 5: ECCLESIASTICAL LATIN MAINTENANCE (PITON) — Church Latin initially resists humanist standards, maintaining medieval forms through institutional inertia. By the Counter-Reformation, ecclesiastical Latin becomes a hybrid: officially classical (Ciceronian forms for prestige), functionally medieval (church ritual and clerical practice use simplified, medieval-influenced forms). The system becomes mostly theatrical — humanist correctness for formal documents, medieval-influenced for actual use. Atrophied function maintained as performance; enforced through institutional authority rather than through epistemic necessity.
constraint_indexing:constraint_classification(sociolinguistic_register_shift, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, Latin's evolution from ancient to medieval to humanist forms appears as an immutable linguistic law: languages necessarily drift over centuries; classical forms are necessarily anterior to medieval forms; recovery requires textual anchorage. The constraint appears as natural linguistic process. However, the presence of beneficiaries (humanist elite) and victims (medieval practitioners) triggers FSM evaluation — the engine will identify this as a false summit, revealing that 'natural linguistic law' naturalizes what is actually a contestable institutional choice about which form counts as 'correct.'
constraint_indexing:constraint_classification(sociolinguistic_register_shift, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sociolinguistic_register_shift_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sociolinguistic_register_shift, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sociolinguistic_register_shift, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(piton_threshold) :-
    domain_priors:theater_ratio(sociolinguistic_register_shift, TR),
    TR >= 0.70.

:- end_tests(sociolinguistic_register_shift_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45): Moderate. The humanist reconstruction extracts significant status, patronage, and intellectual authority from displaced medieval practitioners. However, the extraction is not maximal because (a) genuine epistemic coordination function exists — humanists are solving the real problem of how to read ancient texts correctly, and this provides positive-sum benefit to the intellectual ecosystem; (b) the displacement is generational, not instantaneous — medieval forms persist for decades, allowing gradual transition; (c) alternative pathways exist (ecclesiastical compromise, regional resistance). The value reflects that this is mixed coordination-extraction, not pure predation. Suppression (0.50): Moderate-high. Significant suppression mechanisms operate: institutional gatekeeping (humanist-controlled universities and patronage networks), epistemic delegitimization (medieval forms declared corrupt), career barriers (non-humanist-trained scholars lose prestige), and educational reorientation (old curricula become obsolete). But suppression is not total — medieval forms persist in ecclesiastical contexts, regional schools maintain alternatives, and the transition occurs gradually. Suppression peaks around 1475-1480 as printing standardization and papal support coalesce, then slightly declines as the standard becomes normalized and active enforcement becomes less necessary. Theater ratio (0.68): High and rising. By 1500, the classical Latin standard has become substantially performative. Official documents are written in classical forms while actual ecclesiastical and administrative practice uses simplified medieval-influenced Latin. Humanist grammarians produce increasingly elaborate theoretical justifications for forms that increasingly appear arbitrary to practitioners. The rising trajectory reflects that humanist correctness becomes more about performance and institutional legitimacy and less about communicative necessity.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates the perspectival structure of DR classification with exceptional clarity. The same structural phenomenon — the revaluation of 'correct Latin' from medieval forms to humanist classical forms — appears as snare (to trapped medieval practitioners), tangled rope (to constrained cathedral schools), rope (to beneficiary humanists), scaffold (to organized printers), piton (to ecclesiastical institutions), and mountain (to civilizational observers). The core divergence is in exit options and directionality: those with no exit and victim status experience extraction; those with arbitrage options and beneficiary status experience coordination; those with constrained exit and mixed status experience both. The false summit is the mountain perspective — the analytical observer risks naturalizing the institutional power structure as a natural law of linguistic evolution.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint operates through institutional gatekeeping and epistemic revaluation. Humanist elites occupy institutional positions (university chairs, court positions, papal favor) that allow them to control which form counts as 'correct.' Medieval practitioners have no institutional counter-power. The directionality is not symmetric. Humanists can choose to enforce classical forms or tolerate medieval forms; medieval practitioners cannot choose the standard to which they are held. The beneficiary-victim structure is stable: humanists collect prestige and patronage; medieval practitioners lose legitimacy. The suppression mechanism operates through multiple channels: (1) institutional gatekeeping (humanist-controlled universities), (2) epistemic delegitimization (medieval forms declared 'corrupt'), (3) career barriers (non-humanist training becomes disqualifying), (4) publishing gatekeeping (printers adopt classical standards). The constraint exhibits the classic tangled rope structure: genuine coordination function (accessing ancient texts correctly) layered with asymmetric extraction (one party benefits from status hierarchy while the other is displaced).
 *
 * MANDATROPHY ANALYSIS:
 *   KERNEL OR D1 DISCONTINUITY: The source material suggests that 'correct Latin' might represent either (a) a contested kernel with two incompatible readings (medieval-drift reading vs. classical-recovery reading), or (b) a D1 discontinuity where the reconstructed form was unreachable from the drifted vernacular and required reoccupation from textual symbols. The mandatrophy analysis resolves this by clarifying what the constraint actually enforces. If the constraint is a kernel (contested reading), the two positions are: (1) Medieval Latin is a legitimate natural evolution from ancient forms and should be the standard for contemporary use; (2) Classical Latin is the true form and should be recovered as the standard. These are logically incompatible — they cannot coexist in a single framework. If the constraint is a D1 discontinuity, the positions are: (1) Medieval Latin evolved naturally from ancient forms and is continuous with them; (2) The gulf between medieval and ancient forms is unbridgeable without external (textual) assistance — medieval practitioners could never have reconstructed classical forms through internal linguistic process alone. These are empirically testable rather than logically incompatible. The schema population suggests a kernel reading: the constraint enforces a choice between two incompatible frames, not a discovery of empirical fact. The humanist reading declares medieval Latin corrupt (implicitly: medieval drift reading is illegitimate). The medieval reading (sustained by ecclesiastical practice) continues to assert medieval forms as adequate (implicitly: drift reading is legitimate). The constraint's extraction mechanism operates by suppressing the medieval reading and establishing humanist reading as hegemonic. The mandatrophy is resolved by recognizing that this is tangled rope with a false summit cover: humanist rhetoric naturalizes the choice as discovery of natural law, but the structural data shows institutional power at work. The classical Latin standard is a contestable choice backed by institutional gatekeeping, not a natural law of linguistic correctness.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_recovered_vs_reconstructed,
    'Is Renaissance ''Classical Latin'' a recovery of an actually-used historical form, or a theoretical reconstruction that never existed as a unified living system?',
    'Comparative analysis: what Latin forms appear in manuscripts from the 1st-5th centuries CE (Augustine, Cicero''s texts as copied, Livy) vs. what forms humanists declare ''correct'' in the 15th-16th centuries. If distributions match: recovery. If humanist forms are idealizations beyond what any single ancient speaker used: reconstruction.',
    'If recovery: the humanist project has stronger epistemological warrant — they are accessing actual historical competence. Medieval forms are genuine drift from a real target. If reconstruction: the humanist project is creative archaeology — they are building an ideal form from textual fragments, imposing coherence retroactively. Medieval forms are equally valid as a linguistic system that evolved naturally.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_recovered_vs_reconstructed, empirical, 'Whether humanist Latin is recovery or theoretical reconstruction').

omega_variable(
    natural_law_vs_constructed_standard,
    'Does ''correct Latin'' represent a property of language itself (natural law) or a socially constructed standard imposed through institutional power?',
    'Counterfactual analysis: would medieval Latin be considered ''corrupt'' if humanist elites had not gained institutional power and patronage? Did medieval scribes perceive their Latin as deficient before humanist revaluation, or only after? Historical evidence of pre-humanist medieval Latin self-awareness as ''corrupt'' vs. post-humanist revaluation.',
    'If natural law: the constraint is a mountain — correctness is an inherent property that humanists discovered. If constructed: the constraint is tangled rope with false summit cover — ''correctness'' is a power effect, and the humanist victory is redefining legitimacy through institutional dominance, not discovering truth.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_standard, conceptual, 'Whether Latin correctness is natural property or constructed standard').

omega_variable(
    medieval_practitioners_awareness,
    'Did medieval clerical practitioners experience their Latin as inadequate, or only after humanist revaluation did the inferiority narrative become hegemonic?',
    'Close reading of medieval metalinguistic commentary (grammarians'' own descriptions of their practice); comparison of self-reported competence before and after 15th century humanist campaigns. Did medieval clerics apologize for ''corrupted'' Latin, or did humanists introduce the shame narrative?',
    'If medieval practitioners felt inadequate: internalized suppression was pre-existing; humanists expanded on existing hierarchy. If humanists introduced the shame: the suppression mechanism is creative — humanists had to manufacture the victim''s sense of inferiority to make the extraction work.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(medieval_practitioners_awareness, empirical, 'Whether medieval practitioners internalized inadequacy pre-humanist').

omega_variable(
    falsity_of_humanist_mountain_claim,
    'Is the humanist mountain claim (that Classical Latin correctness is a natural law of language) a genuine conceptual error or a deliberate naturalizing of a power move?',
    'Intellectual history: examine humanist theoretical writings (Petrarch, Poliziano, Valla) on the nature of linguistic correctness. Do they argue for natural law or explicitly for humanist institutional authority? Compare explicit arguments vs. implicit practice (e.g., do they enforce correctness through institutional gatekeeping while claiming natural law?)',
    'If error: humanists genuinely misunderstood the status of their own reconstructions; false summit is a cognitive artifact. If deliberate: humanists knew their claims were institutional power plays and used natural law rhetoric instrumentally — the suppression mechanism includes epistemic capture. The tangled rope classification becomes more acute; the extraction is hidden under false naturalness language.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(falsity_of_humanist_mountain_claim, conceptual, 'Whether humanist mountain claim is error or deliberate naturalizing').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sociolinguistic_register_shift, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(socling_theater_1400, sociolinguistic_register_shift, theater_ratio, 0, 0.35).
narrative_ontology:measurement(socling_theater_1425, sociolinguistic_register_shift, theater_ratio, 25, 0.52).
narrative_ontology:measurement(socling_theater_1450, sociolinguistic_register_shift, theater_ratio, 50, 0.68).
narrative_ontology:measurement(socling_theater_1475, sociolinguistic_register_shift, theater_ratio, 75, 0.75).
narrative_ontology:measurement(socling_theater_1500, sociolinguistic_register_shift, theater_ratio, 100, 0.82).

% Extraction over time
narrative_ontology:measurement(socling_extract_1400, sociolinguistic_register_shift, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(socling_extract_1425, sociolinguistic_register_shift, base_extractiveness, 25, 0.35).
narrative_ontology:measurement(socling_extract_1450, sociolinguistic_register_shift, base_extractiveness, 50, 0.42).
narrative_ontology:measurement(socling_extract_1475, sociolinguistic_register_shift, base_extractiveness, 75, 0.48).
narrative_ontology:measurement(socling_extract_1500, sociolinguistic_register_shift, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(socling_suppress_1400, sociolinguistic_register_shift, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(socling_suppress_1425, sociolinguistic_register_shift, suppression_requirement, 25, 0.42).
narrative_ontology:measurement(socling_suppress_1450, sociolinguistic_register_shift, suppression_requirement, 50, 0.55).
narrative_ontology:measurement(socling_suppress_1475, sociolinguistic_register_shift, suppression_requirement, 75, 0.62).
narrative_ontology:measurement(socling_suppress_1500, sociolinguistic_register_shift, suppression_requirement, 100, 0.5).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sociolinguistic_register_shift, information_standard).
narrative_ontology:affects_constraint(sociolinguistic_register_shift, ecclesiastical_latin_standardization).
narrative_ontology:affects_constraint(sociolinguistic_register_shift, printing_orthographic_codification).
narrative_ontology:affects_constraint(sociolinguistic_register_shift, humanist_educational_gatekeeping).

% DUAL FORMULATION NOTE:
% The sociolinguistic register shift is downstream of three structurally distinct coordination problems: (1) ecclesiastical_latin_standardization — how church institutions maintain Latin across regional variation; (2) printing_orthographic_codification — how printers achieve consistency across distributed production; (3) humanist_educational_gatekeeping — how humanist elites control access to prestige positions. Each has different epsilon values reflecting different coordination costs and extraction mechanisms. The register shift is the intersection of all three, where extractive choice in educational gatekeeping becomes layered onto coordination problems in ecclesiastical standardization and printing orthography.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
