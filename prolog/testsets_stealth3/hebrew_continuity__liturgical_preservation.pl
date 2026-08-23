% ============================================================================
% CONSTRAINT STORY: hebrew_continuity__liturgical_preservation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_hebrew_continuity__liturgical_preservation, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: hebrew_continuity__liturgical_preservation
 *   human_readable: Liturgical Preservation of Hebrew (Ritual Recitation and Textual Transmission)
 *   domain: sociolinguistic/religious/commitment-system
 *
 * SUMMARY:
 *   From roughly the second century CE, when Hebrew ceased to be a
 *   vernacular, until the eve of the spoken revival (1880), the language was
 *   kept alive — in the sense this reading endorses — by ritual recitation
 *   and textual transmission: a fixed canon, a standardized liturgy, and a
 *   transnational school regime through which every community child learned
 *   to decode and recite texts nobody around them spoke. This story
 *   instantiates the liturgical_preservation reading of the hebrew_continuity
 *   kernel and assesses THAT standing arrangement by the reading's own
 *   lights: a genuine coordination regime (pan-diaspora canon, law, liturgy,
 *   correspondence) that also extracted real costs — childhood acquisition
 *   labor without comprehension, communal enforcement against
 *   vernacular-liturgy and secularizing alternatives, and concentrated
 *   custodial authority. The epsilon referent is the liturgical arrangement
 *   itself, not the later native-speaker revival this reading did not seek.
 *   The reading's characteristic victim set — secularizing forces threatening
 *   the textual tradition (reformers, maskilim) — is declared alongside the
 *   arrangement's internal cost-bearers (the students). This is one of three
 *   linked stories decomposing the kernel: the native_generative reading
 *   makes unbroken native transmission constitutive (under it these centuries
 *   are managed language death and epsilon is assessed on a different
 *   arrangement), and the bridge_pidginized reading re-describes the same
 *   centuries as contact-language maintenance; the three share the historical
 *   record but assign different victim sets and different epsilon, so they
 *   are separate stories linked by network.affects_constraints, not one story
 *   with a measurement parameter. Claim and metrics are independent: the
 *   claimed type states what I take to be structurally true; the metrics
 *   describe the arrangement's actual operation.
 *
 * KEY AGENTS:
 *   - rabbinic_leadership: agenda-setting custodian (institutional/identity_locked) — fixes liturgy and curriculum, enforces the regime, collects interpretive authority
 *   - diaspora_communities: primary beneficiary (organized/constrained) — receives canon, liturgy, and mutual recognition; funds and polices the regime
 *   - hebrew_students: primary cost-bearer (powerless/trapped) — childhood labor converted into recitation competence without communicative return
 *   - vernacular_liturgy_reformers: suppressed alternative (organized/constrained) — bears bans and schism for vernacular worship
 *   - secularizing_maskilim: suppressed alternative (moderate/mobile) — bears censorship and exit for secularizing Hebrew or replacing it
 *   - women_excluded_from_text_study: excluded constituency (powerless/trapped) — outside the councils, largely outside Hebrew literacy
 *   - modern_sociolinguists: analytical observer (analytical/analytical) — reads the regime as the long case of language-without-native-speakers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, 0.58).
domain_priors:suppression_score(hebrew_continuity__liturgical_preservation, 0.58).
domain_priors:theater_ratio(hebrew_continuity__liturgical_preservation, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, extractiveness, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(hebrew_continuity__liturgical_preservation, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(hebrew_continuity__liturgical_preservation, tangled_rope).
narrative_ontology:human_readable(hebrew_continuity__liturgical_preservation, "Liturgical Preservation of Hebrew (Ritual Recitation and Textual Transmission)").
narrative_ontology:topic_domain(hebrew_continuity__liturgical_preservation, "sociolinguistic/religious/commitment-system").

domain_priors:requires_active_enforcement(hebrew_continuity__liturgical_preservation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(hebrew_continuity__liturgical_preservation, 'b5f7550f-ffe1-42d1-ac7e-6ee951982445').
narrative_ontology:cs_kernel_codification('b5f7550f-ffe1-42d1-ac7e-6ee951982445', fixed_text).
narrative_ontology:cs_authority_grounding('b5f7550f-ffe1-42d1-ac7e-6ee951982445', lineage).
narrative_ontology:cs_interpretation_layer_present('b5f7550f-ffe1-42d1-ac7e-6ee951982445').
narrative_ontology:cs_reading_relation('b5f7550f-ffe1-42d1-ac7e-6ee951982445', hebrew_continuity__native_generative, forecloses).
narrative_ontology:cs_reading_relation('b5f7550f-ffe1-42d1-ac7e-6ee951982445', hebrew_continuity__bridge_pidginized, influences).
narrative_ontology:cs_axiom('b5f7550f-ffe1-42d1-ac7e-6ee951982445', foundational, recitation_suffices_for_continuity).
narrative_ontology:cs_axiom_status(recitation_suffices_for_continuity, holdable).
narrative_ontology:cs_axiom_grounding('b5f7550f-ffe1-42d1-ac7e-6ee951982445', recitation_suffices_for_continuity, theological).
narrative_ontology:cs_axiom('b5f7550f-ffe1-42d1-ac7e-6ee951982445', secondary, fixed_hebrew_liturgy_obligatory).
narrative_ontology:cs_axiom_status(fixed_hebrew_liturgy_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('b5f7550f-ffe1-42d1-ac7e-6ee951982445', fixed_hebrew_liturgy_obligatory, conventional).
narrative_ontology:cs_reference_frame('b5f7550f-ffe1-42d1-ac7e-6ee951982445', masoretic_liturgical_continuity).
narrative_ontology:cs_drift_state('b5f7550f-ffe1-42d1-ac7e-6ee951982445', eve_of_revival, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('b5f7550f-ffe1-42d1-ac7e-6ee951982445', '').
narrative_ontology:cs_kernel_id(hebrew_continuity__liturgical_preservation, hebrew_continuity).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, rabbinic_leadership).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, diaspora_communities).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, hebrew_students).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, vernacular_liturgy_reformers).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, secularizing_maskilim).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(hebrew_continuity__liturgical_preservation, hebrew_students).
narrative_ontology:constraint_victim(hebrew_continuity__liturgical_preservation, diaspora_communities).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, leshon_hakodesh_sanctity_doctrine).
narrative_ontology:constraint_vindicates(hebrew_continuity__liturgical_preservation, masoretic_transmission_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Fixes the liturgy, curates the canon, and sets the curriculum through which every community child encounters Hebrew; administers the communal courts and the disciplinary instruments (herem, fines, schooling compulsion) that hold the recitation regime in place. Their standing, livelihood, and interpretive authority are constituted by the custodianship itself — stepping outside it would dissolve the role, not merely the income.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, rabbinic_leadership, agenda_setter,
    institutional, generational, identity_locked, global).

% Scattered communities from Cairo to Cologne to Cochin read the same scroll, recite the same fixed prayer, and recognize one another's documents and correspondence because the liturgical-textual regime holds. They fund the schools and bear the costs of communal discipline. Leaving the regime — through conversion, assimilation, or schism — meant losing mutual aid, the marriage network, and legal standing, so exit existed but at the price of communal death.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, diaspora_communities, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, diaspora_communities, payer).

% Children enrolled in heder and yeshiva spend their childhood decoding and memorizing a language nobody around them speaks; by late in the interval most graduate able to recite and parse far more than they can understand or produce. They consent to nothing and cannot leave; the communal standing they eventually receive is the benefit side of a bill they never saw.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, hebrew_students, payer,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(hebrew_continuity__liturgical_preservation, hebrew_students, beneficiary).

% From the Karaite rupture through the Hamburg Temple dispute, movements that wanted prayer in the vernacular or liturgical innovation met communal courts, bans, and schism. Some exited and built new congregations at the cost of family rupture and excommunication; most dissent was absorbed back into recitation. Their preferred alternative — a Judaism prayed in the language of the street — is precisely what the enforcement machinery exists to prevent.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, vernacular_liturgy_reformers, payer,
    organized, biographical, constrained, continental).

% Enlightenment-era intellectuals who wanted Hebrew modernized into a secular print language, or replaced by European vernaculars, operated inside communities whose schools, presses, and marriage networks the custodianship controlled. Many were banned and their books censored or burned; the mobile ones exited into secular European life, which the custodianship read as confirmation that the alternative was assimilation rather than a rival mode of continuity.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, secularizing_maskilim, payer,
    moderate, biographical, mobile, continental).

% In most communities and across most of the interval, girls were not taught Hebrew text; they prayed in the vernacular of their kitchens or followed a service they could not read. The regime's educational budget, its councils, and its interpretive authority were all male; the largest constituency inside the communities had no seat where the arrangement was set and would have objected that recitation without comprehension was the only Hebrew they were ever offered.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, women_excluded_from_text_study, excluded,
    powerless, biographical, trapped, global).

% Scholars of language death, diglossia, and revitalization examine the liturgical-preservation regime as the longest-running case of a language sustained for centuries without native speakers; they take the masoretic apparatus, the communal records, and the revival's raw material as data, and hold no seat in any of the communities they describe.
narrative_ontology:constraint_stakeholder(hebrew_continuity__liturgical_preservation, modern_sociolinguists, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(hebrew_continuity__liturgical_preservation, rabbinic_leadership).
narrative_ontology:fixing_cost_class(hebrew_continuity__liturgical_preservation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single canon, liturgy, and legal-linguistic register across a stateless, multi-vernacular diaspora: fixed text, standardized recitation, and a transnational school system let communities that could not converse still read, pray, adjudicate, and correspond in common.
% TRANSFER_FUNCTION: Moves children's years of study labor and communal funds into acquisition of recitation competence; moves interpretive authority, status, and institutional control to the rabbinic custodianship; and moves dissent over language choice out of the community, through ban or schism.
% ABSENT_VOICES: Women, excluded from Hebrew text instruction, and the children themselves, who bore the acquisition burden without consent, would object if seated; so would the vernacular-preference majorities in communities where comprehension of recited Hebrew had collapsed. They are absent because the councils that set curriculum, liturgy, and discipline were restricted to the male learned elite.
% DISAPPEARANCE_RATIONALE: If the recitation-and-transmission regime vanished overnight at any point in the interval, the diaspora's shared canon, liturgical unity, legal correspondence, and communal boundary would unravel within generations — and the raw material on which the later spoken revival drew (the fixed text, the pronunciation traditions, the pan-diaspora literate class) would not exist to be revived.
% FOUNDING_PROBLEM: After Hebrew ceased to be anyone's vernacular (roughly the second to third century CE), a dispersed minority with no territory, no common spoken language, and no sovereign institutions needed a way to keep one canon, one law, and one liturgy recognizable from Babylon to the Rhineland across a hundred generations.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: the Cairo Geniza documents and medieval responsa show geographically separated communities adjudicating and corresponding in Hebrew and Aramaic; European philologists and later the revivalists themselves attest that the preserved corpus existed and was usable — Ben-Yehuda's revival drew directly on it. No source inside or outside the tradition attests that the problem had been solved by other means during the interval; the maskilim corroborate the problem's existence while disputing the liturgical monopoly as its solution.
narrative_ontology:disappearance_verdict(hebrew_continuity__liturgical_preservation, world_rearranges).
narrative_ontology:founding_problem_status(hebrew_continuity__liturgical_preservation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(hebrew_continuity__liturgical_preservation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(hebrew_continuity__liturgical_preservation, 'none', 1).
narrative_ontology:epsilon_provenance(hebrew_continuity__liturgical_preservation, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hebrew_continuity__liturgical_preservation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(hebrew_continuity__liturgical_preservation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(hebrew_continuity__liturgical_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.58 at interval end) reflects the arrangement's core cost: by the late interval most reciters could decode and perform far more than they understood or could produce, so each generation's acquisition labor bought less communicative return than the last — a rising series (0.30 to 0.58) driven by vernacular shift rather than growing rapacity. Suppression (0.58) is structural first: herem, communal courts, school compulsion, and censorship held vernacular-liturgy and secularizing alternatives down; the enforcement series traces one full arc — institutionalizing through the geonic era, peaking with the medieval kehillah's autonomy (0.56), eroding as state emancipation stripped communal coercion (0.50), then flaring (0.58) where the custodianship still held power and the reform and Haskalah challenges arrived. Theater (0.35) honestly credits rote recitation's growing performative share while the masoretic, legal, and exegetical functions stayed real. Accessibility collapse (0.45) and resistance (0.50) sit where a long-lived construct with workable-but-costly alternatives sits: Karaism, vernacular liturgy, secularization, and exit all remained possible and were periodically exercised at the price of schism or communal death. All three tracked series share one time grid (200, 500, 800, 1100, 1400, 1700, 1880) so the engine samples every metric at every authored point.
 *
 * PERSPECTIVAL GAP:
 *   The custodian seat and the student seat should compute differently and do: from rabbinic_leadership the arrangement is the coordination it administers and the continuity it embodies (beneficiary end, identity_locked — the role cannot be exited without dissolving itself); from hebrew_students it is childhood labor spent on a code with no communicative payoff (trapped, powerless — full-target end). diaspora_communities straddle: net beneficiaries who also fund and police the regime. The dissenter seats compute suppression rather than tribute — their cost is the closed alternative, not a levied payment. The observer seat sees a seventeen-hundred-year diglossia and holds no stake. The engine computes these divergences from the structural data; the authored claim does not adjudicate them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to real flows: the custodianship receives interpretive authority and institutional control (d near the beneficiary end); the communities receive canon, liturgy, and mutual recognition while paying funding and enforcement costs (low d, damped toward symmetric by the payer secondary role). Victim declarations map the cost side: students bear acquisition labor with trapped exit (near full target); reformers bear bans and schism (organized, constrained — high d); maskilim bear censorship and exit (mobile exit damps d slightly). Women excluded from text study sit outside the beneficiary/victim derivation entirely — the arrangement's largest unconsulted constituency. No directionality overrides were needed: beneficiary/victim declarations plus exit options place every seat where the structural record puts it.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards both mislabelings this case invites. Reading the arrangement as pure rope (the tradition's own framing: identity coordination, everyone net-benefits, no coercion) ignores the victim declarations and the enforcement record — herem, school compulsion, censorship — that the coordination rode on. Reading it as pure snare (the Haskalah critique: a clerisy extracting labor and crushing alternatives) ignores that the coordination function is genuine and primary — the pan-diaspora canon, legal correspondence, and liturgical unity were real goods this arrangement alone delivered across a stateless diaspora, and removing it does not merely free the victims, it dissolves the continuity. Tangled_rope holds both truths: coordination and extraction through the same structure, held by active enforcement. On the R5 interview the founding problem stayed live for the entire interval, so no zombie flag is available here: whatever atrophy came later belongs to the post-revival stories, not this one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_under_determination,
    'This constraint is one reading of the hebrew_continuity kernel; would instantiating a sibling reading change the standing arrangement''s classification?',
    'Generate the sibling stories (hebrew_continuity__native_generative, hebrew_continuity__bridge_pidginized) and compare computed types over the same interval: native_generative would read the liturgical centuries as managed language death (raising extraction, shifting victims toward the generations denied native acquisition); bridge_pidginized would re-describe the coordination function as contact-language maintenance (broadening the beneficiary set and damping the identity-coordination reading).',
    'Under native_generative the arrangement likely computes more extractive; under bridge_pidginized less. This story''s tangled_rope verdict is reading-indexed, not topic-absolute.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Committer structure: which reading of the continuity kernel is instantiated changes victim sets and epsilon.').

omega_variable(
    comprehension_decay_irreversibility,
    'Was the decay of reciter comprehension — and hence the rising acquisition cost per unit of understanding — a structural consequence of the liturgical-preservation arrangement, or a contingent product of specific curricular choices?',
    'Comparative historical analysis: Sephardi and Italian communities maintained higher Hebrew comprehension longer than Ashkenaz; compare curriculum records, responsa complaints about incomprehension, and maskilic critiques across communities.',
    'If structural, the rising base_extractiveness series is built into the arrangement and any revival required breaking it; if contingent, the arrangement could in principle have sustained comprehension and part of the measured extraction is remediable pedagogy rather than regime cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(comprehension_decay_irreversibility, empirical, 'Whether rising extraction over the interval was built-in or contingent.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of vernacular-liturgy alternatives structural (communal discipline, herem, school control) or internalized (the sanctity conviction that Hebrew prayer is the only legitimate mode)?',
    'Post-emancipation trajectory: as state law stripped kehillah coercive power from the late eighteenth century onward, communities that could have switched to vernacular liturgy at falling cost mostly did not — persistence under removed enforcement indicates a substantial internalized component.',
    'If largely internalized, suppression persists without enforcement machinery and the constraint''s effective suppression exceeds what the structural record shows; the reform-era enforcement flare at the interval end would be the visible remnant of a mostly internalized regime.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized share of the suppression holding vernacular alternatives down.').

omega_variable(
    dissenter_jurisdiction_ambiguity,
    'Are the vernacular reformers and maskilim victims of this constraint (governed parties whose alternatives are suppressed) or external challengers to it (against whom suppression is boundary defense rather than extraction)?',
    'Trace jurisdiction at the moment of suppression: reformers and maskilim were, when suppressed, still inside the communities — married, taxed, and schooled under kehillah authority; exit typically followed suppression rather than preceding it.',
    'If they count as governed parties, the suppression component is extraction from the governed and supports the tangled_rope verdict; if external challengers, the same acts read as boundary defense and the arrangement moves toward rope with a defended border.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(dissenter_jurisdiction_ambiguity, conceptual, 'Whether the suppressed dissenters sit inside the constraint''s jurisdiction (victims) or outside it (challengers).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(hebrew_continuity__liturgical_preservation, 200, 1880).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hebr_tr_t200, hebrew_continuity__liturgical_preservation, theater_ratio, 200, 0.1).
narrative_ontology:measurement_basis(hebr_tr_t200, observed).
narrative_ontology:measurement(hebr_tr_t500, hebrew_continuity__liturgical_preservation, theater_ratio, 500, 0.14).
narrative_ontology:measurement_basis(hebr_tr_t500, observed).
narrative_ontology:measurement(hebr_tr_t800, hebrew_continuity__liturgical_preservation, theater_ratio, 800, 0.18).
narrative_ontology:measurement_basis(hebr_tr_t800, observed).
narrative_ontology:measurement(hebr_tr_t1100, hebrew_continuity__liturgical_preservation, theater_ratio, 1100, 0.22).
narrative_ontology:measurement_basis(hebr_tr_t1100, observed).
narrative_ontology:measurement(hebr_tr_t1400, hebrew_continuity__liturgical_preservation, theater_ratio, 1400, 0.28).
narrative_ontology:measurement_basis(hebr_tr_t1400, observed).
narrative_ontology:measurement(hebr_tr_t1700, hebrew_continuity__liturgical_preservation, theater_ratio, 1700, 0.32).
narrative_ontology:measurement_basis(hebr_tr_t1700, observed).
narrative_ontology:measurement(hebr_tr_t1880, hebrew_continuity__liturgical_preservation, theater_ratio, 1880, 0.35).
narrative_ontology:measurement_basis(hebr_tr_t1880, observed).

% Extraction over time
narrative_ontology:measurement(hebr_be_t200, hebrew_continuity__liturgical_preservation, base_extractiveness, 200, 0.3).
narrative_ontology:measurement_basis(hebr_be_t200, observed).
narrative_ontology:measurement(hebr_be_t500, hebrew_continuity__liturgical_preservation, base_extractiveness, 500, 0.35).
narrative_ontology:measurement_basis(hebr_be_t500, observed).
narrative_ontology:measurement(hebr_be_t800, hebrew_continuity__liturgical_preservation, base_extractiveness, 800, 0.42).
narrative_ontology:measurement_basis(hebr_be_t800, observed).
narrative_ontology:measurement(hebr_be_t1100, hebrew_continuity__liturgical_preservation, base_extractiveness, 1100, 0.48).
narrative_ontology:measurement_basis(hebr_be_t1100, observed).
narrative_ontology:measurement(hebr_be_t1400, hebrew_continuity__liturgical_preservation, base_extractiveness, 1400, 0.52).
narrative_ontology:measurement_basis(hebr_be_t1400, observed).
narrative_ontology:measurement(hebr_be_t1700, hebrew_continuity__liturgical_preservation, base_extractiveness, 1700, 0.55).
narrative_ontology:measurement_basis(hebr_be_t1700, observed).
narrative_ontology:measurement(hebr_be_t1880, hebrew_continuity__liturgical_preservation, base_extractiveness, 1880, 0.58).
narrative_ontology:measurement_basis(hebr_be_t1880, observed).

% Suppression requirement over time
narrative_ontology:measurement(hebr_su_t200, hebrew_continuity__liturgical_preservation, suppression_requirement, 200, 0.32).
narrative_ontology:measurement_basis(hebr_su_t200, observed).
narrative_ontology:measurement(hebr_su_t500, hebrew_continuity__liturgical_preservation, suppression_requirement, 500, 0.4).
narrative_ontology:measurement_basis(hebr_su_t500, observed).
narrative_ontology:measurement(hebr_su_t800, hebrew_continuity__liturgical_preservation, suppression_requirement, 800, 0.48).
narrative_ontology:measurement_basis(hebr_su_t800, observed).
narrative_ontology:measurement(hebr_su_t1100, hebrew_continuity__liturgical_preservation, suppression_requirement, 1100, 0.56).
narrative_ontology:measurement_basis(hebr_su_t1100, observed).
narrative_ontology:measurement(hebr_su_t1400, hebrew_continuity__liturgical_preservation, suppression_requirement, 1400, 0.52).
narrative_ontology:measurement_basis(hebr_su_t1400, observed).
narrative_ontology:measurement(hebr_su_t1700, hebrew_continuity__liturgical_preservation, suppression_requirement, 1700, 0.5).
narrative_ontology:measurement_basis(hebr_su_t1700, observed).
narrative_ontology:measurement(hebr_su_t1880, hebrew_continuity__liturgical_preservation, suppression_requirement, 1880, 0.58).
narrative_ontology:measurement_basis(hebr_su_t1880, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(hebrew_continuity__liturgical_preservation, identity_coordination).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__native_generative).
narrative_ontology:affects_constraint(hebrew_continuity__liturgical_preservation, hebrew_continuity__bridge_pidginized).

% DUAL FORMULATION NOTE:
% Kernel decomposition: 'Hebrew continuity' is one contested kernel instantiated as three structurally distinct constraints. This story instantiates liturgical_preservation (recitation plus textual transmission suffice; zero native speakers required; epsilon assessed on the standing liturgical arrangement, which the reading sees as moderately extractive — real pedagogical and suppression costs carried as the price of continuity). The native_generative reading (hebrew_continuity__native_generative) makes unbroken native generative transmission the constitutive claim — under it the liturgical centuries are language death managed symbolically, and the epsilon referent shifts to whatever arrangement that reading contests. The bridge_pidginized reading (hebrew_continuity__bridge_pidginized) re-describes the same centuries as contact-language maintenance. The three readings share the historical record but assign different victim sets and different epsilon; they are separate stories linked here, not one story with a measurement parameter.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
