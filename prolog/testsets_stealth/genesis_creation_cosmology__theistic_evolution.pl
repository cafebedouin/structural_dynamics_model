% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__theistic_evolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_cosmology__theistic_evolution, []).

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
 *   constraint_id: genesis_creation_cosmology__theistic_evolution
 *   human_readable: Non-Literal Genesis Hermeneutic Compatible with Evolutionary Cosmology
 *   domain: religious/theological/philosophy_of_science
 *
 * SUMMARY:
 *   This story instantiates one reading — theistic_evolution — of the
 *   contested kernel genesis_creation_cosmology: the commitment that Genesis
 *   1-2 carries binding authority over origins discourse. Under this reading,
 *   the operative norm within adopting communities (mainline Protestant
 *   bodies, Catholic teaching since the mid-twentieth century, and a growing
 *   evangelical wing) is that Genesis conveys theological truth — creation's
 *   dependence on God, the goodness of the created order, human dignity and
 *   vocation — through non-literal Ancient Near Eastern literary forms, and
 *   therefore stands fully compatible with evolutionary cosmology. The norm
 *   is presented as fidelity to the text's own genre; it functions to
 *   reconcile scriptural authority with the scientific account of cosmic and
 *   biological history. Its operation coordinates the faith-and-science
 *   settlement for the majority while transferring interpretive jurisdiction
 *   over the text from every reader's plain sense to credentialed
 *   specialists, and while demoting the inherited literal reading — and those
 *   who hold it — inside adopting institutions. The sibling readings
 *   (young_earth_literal, literary_framework) are separate constraints with
 *   their own epsilon values, victim sets, and enforcement burdens; they are
 *   neither described nor averaged here. KEY AGENTS (by structural
 *   relationship): mainline_denominations: agenda setter
 *   (institutional/arbitrage) — sets and administers the interpretive norm;
 *   seminary_faculties: agenda setter and beneficiary (institutional/mobile)
 *   — run the credentialing gate; academic_biblical_scholars: primary
 *   beneficiary (organized/mobile) — supply the genre scholarship that
 *   warrants the reading; credentialed_clergy: beneficiary and payer
 *   (organized/constrained) — exercise the licensed interpretive authority;
 *   natural_scientific_community: incidental beneficiary
 *   (institutional/arbitrage) — holds natural history uncontested;
 *   scientifically_informed_believers: beneficiary (moderate/constrained) —
 *   spared the forced choice; plain_sense_lay_readers: payer
 *   (powerless/constrained) — their face-value reading is ruled insufficient;
 *   literalist_tradition_adherents: payer and excluded voice
 *   (organized/mobile) — the demoted inherited constituency.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__theistic_evolution, 0.42).
domain_priors:suppression_score(genesis_creation_cosmology__theistic_evolution, 0.4).
domain_priors:theater_ratio(genesis_creation_cosmology__theistic_evolution, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, extractiveness, 0.42).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, theater_ratio, 0.25).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__theistic_evolution, tangled_rope).
narrative_ontology:human_readable(genesis_creation_cosmology__theistic_evolution, "Non-Literal Genesis Hermeneutic Compatible with Evolutionary Cosmology").
narrative_ontology:topic_domain(genesis_creation_cosmology__theistic_evolution, "religious/theological/philosophy_of_science").

domain_priors:requires_active_enforcement(genesis_creation_cosmology__theistic_evolution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__theistic_evolution, '66a144aa-1d5e-48ac-a700-2a5887136752').
narrative_ontology:cs_kernel_codification('66a144aa-1d5e-48ac-a700-2a5887136752', fixed_text).
narrative_ontology:cs_authority_grounding('66a144aa-1d5e-48ac-a700-2a5887136752', lineage).
narrative_ontology:cs_interpretation_layer_present('66a144aa-1d5e-48ac-a700-2a5887136752').
narrative_ontology:cs_reading_relation('66a144aa-1d5e-48ac-a700-2a5887136752', genesis_creation_cosmology__young_earth_literal, forecloses).
narrative_ontology:cs_reading_relation('66a144aa-1d5e-48ac-a700-2a5887136752', genesis_creation_cosmology__literary_framework, coexists_with).
narrative_ontology:cs_axiom('66a144aa-1d5e-48ac-a700-2a5887136752', foundational, genesis_conveys_theology_not_cosmology).
narrative_ontology:cs_axiom_status(genesis_conveys_theology_not_cosmology, holdable).
narrative_ontology:cs_axiom_grounding('66a144aa-1d5e-48ac-a700-2a5887136752', genesis_conveys_theology_not_cosmology, theological).
narrative_ontology:cs_axiom('66a144aa-1d5e-48ac-a700-2a5887136752', foundational, no_forced_choice_between_scripture_and_science).
narrative_ontology:cs_axiom_status(no_forced_choice_between_scripture_and_science, holdable).
narrative_ontology:cs_axiom_grounding('66a144aa-1d5e-48ac-a700-2a5887136752', no_forced_choice_between_scripture_and_science, instrumental).
narrative_ontology:cs_reference_frame('66a144aa-1d5e-48ac-a700-2a5887136752', accommodated_theological_revelation).
narrative_ontology:cs_drift_state('66a144aa-1d5e-48ac-a700-2a5887136752', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('66a144aa-1d5e-48ac-a700-2a5887136752', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, mainline_denominations).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, seminary_faculties).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, academic_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, credentialed_clergy).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, natural_scientific_community).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, scientifically_informed_believers).
narrative_ontology:constraint_victim(genesis_creation_cosmology__theistic_evolution, plain_sense_lay_readers).
narrative_ontology:constraint_victim(genesis_creation_cosmology__theistic_evolution, literalist_tradition_adherents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(genesis_creation_cosmology__theistic_evolution, credentialed_clergy).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__theistic_evolution, ancient_near_eastern_genre_hypothesis).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__theistic_evolution, evolutionary_cosmology).
narrative_ontology:constraint_vindicates(genesis_creation_cosmology__theistic_evolution, two_books_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets the interpretive norm through confessional statements, educational materials, and ordination requirements, and operates the seminary system that trains clergy in historical-critical genre analysis. Collects institutional credibility with educated publics, legal establishments, and ecumenical partners. Pays enforcement costs when literalist factions contest, and can revise the norm through synods and assemblies at will.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, mainline_denominations, agenda_setter,
    institutional, generational, arbitrage, continental).

% Designs the curricula that teach Genesis 1-11 as Ancient Near Eastern literature and certifies candidates for ordination. The certification gate is the practical enforcement point where face-value readings are corrected before graduates reach pulpits. Collects tuition, prestige, and publishing careers; individual faculty can move between religious and secular institutions.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, seminary_faculties, agenda_setter,
    institutional, generational, mobile, continental).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__theistic_evolution, seminary_faculties, beneficiary).

% Produces the genre analyses, comparative Ancient Near Eastern studies, and commentaries that warrant the non-literal reading. The arrangement converts their specialized knowledge into interpretive authority over the text for millions of readers. Their professional standing does not depend on the norm's survival, since the same expertise is valued in secular academia.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, academic_biblical_scholars, beneficiary,
    organized, biographical, mobile, global).

% Exercises licensed interpretive authority week by week, preaching and teaching the non-literal reading to congregations. Collects vocational role and standing as the accredited mediators of the text's meaning. Pays years of seminary training and the ongoing labor of managing congregants who read the text at face value. Exit means leaving the vocation.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, credentialed_clergy, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__theistic_evolution, credentialed_clergy, payer).

% Holds jurisdiction over cosmic and biological history uncontested by mainstream theology under this reading. Does not administer the norm and would proceed identically without it, but collects the removal of scriptural counterclaims to its findings. Engages the religious world through courtesy dialogue initiatives rather than through the enforcing machinery.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, natural_scientific_community, beneficiary,
    institutional, generational, arbitrage, global).

% Members of believing communities with scientific education who would otherwise face a forced choice between their faith and their knowledge of cosmology and evolution. The reading lets them remain in both. They pay deference to specialist interpretation they rarely verify personally, trusting the credentialing chain.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, scientifically_informed_believers, beneficiary,
    moderate, biographical, constrained, global).

% Read Genesis in translation at face value and are told their reading is naive. Must accept the specialist hermeneutic on trust or self-exclude from educated religious life. Their interpretive autonomy moves upward to the credentialed classes. Exit means leaving their congregation, or remaining while carrying unresolved dissonance between the text as they read it and the meaning they are told it has.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, plain_sense_lay_readers, payer,
    powerless, biographical, constrained, regional).

% Hold the inherited plain reading as binding on the text. Inside adopting institutions they are marginalized: excluded from teaching posts, corrected from pulpits, treated as embarrassing kin, and ineligible for the credentialing gate. They sustain a parallel institutional ecosystem — creationist ministries, schools, museums, publications — where their reading governs, and exit from the mainline bodies is available and increasingly taken across generations.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, literalist_tradition_adherents, payer,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__theistic_evolution, literalist_tradition_adherents, excluded).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_cosmology__theistic_evolution, credentialed_clergy).
narrative_ontology:fixing_cost_class(genesis_creation_cosmology__theistic_evolution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the epistemic collision between scriptural authority and modern science for believing communities: it assigns Genesis 1-2 to theological instruction delivered through recognized ancient literary forms, and assigns cosmic and biological history to scientific investigation, so members can inhabit both commitments without contradiction.
% TRANSFER_FUNCTION: Moves interpretive jurisdiction over Genesis from every reader's plain sense to credentialed specialists (genre-trained clergy and biblical scholars); moves the labor of reconciliation onto a hermeneutic that laity must accept on trust; and moves standing within adopting institutions away from holders of the inherited literal reading.
% ABSENT_VOICES: Young-earth creationist scholars and plain-sense lay readers are largely absent from the seminary classrooms, confessional committees, and academic journals where the reading is ratified; they speak from parallel institutions that the ratifying bodies do not seat as peers. Their core objection — that the non-literal reading concedes the text's authority precisely where it claims to defend it — is recorded mainly by opponents of the arrangement.
% DISAPPEARANCE_RATIONALE: If the norm vanished overnight, mainline denominations would face immediate rupture: seminaries would split over curriculum, scientifically educated members would defect or retreat into compartmentalization, clergy trained in the genre-critical hermeneutic would lose their operative warrant, and the institutional map of European and American Protestantism would reorganize around the reopened literal-versus-scientific fault line.
% FOUNDING_PROBLEM: After geological deep time and Darwin's Origin of Species (1859), the plain literal reading of Genesis collided with established science, forcing educated believers toward a choice between intellectual honesty and scriptural fidelity; the non-literal reading was built to dissolve that forced choice.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: secular historians of the Victorian crisis of faith and of the American fundamentalist-modernist controversy document the collision and its institutional aftermath in detail; creationist organizations attest the same conflict adversarially, agreeing it is real while rejecting the accommodation; no serious party denies that the collision occurred or that this reading was constructed in response to it.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__theistic_evolution, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__theistic_evolution, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__theistic_evolution, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_cosmology__theistic_evolution, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_cosmology__theistic_evolution, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_cosmology__theistic_evolution_tests).
:- end_tests(genesis_creation_cosmology__theistic_evolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.42): the arrangement imposes real but bounded costs — lay deference to unverified specialist judgment, loss of textual jurisdiction over natural history, and demotion of the literalist minority — against a large coordination dividend (faith-science coherence for millions of believers). Suppression (0.40) is institutional-pedagogical rather than coercive: credentialing gates, curricular control, and pulpit correction, with alternatives remaining openly available outside adopting bodies. Theater ratio (0.25) reflects a genuinely functional genre-scholarship core plus a performative shell of scriptural-affirmation rhetoric that functions reputationally while operative authority sits in the hermeneutic. Accessibility collapse is low (0.30): understanding the norm does not close exits, because flourishing parallel creationist institutions remain visible and joinable. Resistance (0.50) is sustained and organized. The temporal series runs on one shared grid (t = 0, 30, 60, 90, 120, 150, 165 years from 1859) and traces one full liberal-conservative cycle: crisis and early accommodation (t0-t30), fundamentalist backlash and the enforcement ratchet of the modernist controversy era (t60), post-war consolidation and peak bureaucratic enforcement (t90-t120), then renewed contest and partial relaxation as evangelical accommodation expands (t150-t165). The endpoint metrics were measured in the renewed-contestation phase of the cycle; the oscillation tracks external intellectual cycles (Darwin, Fundamentalism, the neo-Darwinian synthesis, creationist institutionalization, the genome era) rather than functioning as intermittent reinforcement.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the setter/beneficiary seats should compute differently. From the denominational and scholarly seats the norm is fidelity: the text's own genre, honestly read, harmonized with God's other book. From the plain-sense laity seat the same structure operates as a deference tax — their reading is ruled insufficient by credentials they cannot inspect. From the literalist seat it is dispossession: an inherited reading demoted inside the institutions their tradition built. Identity-lock differs by seat: literalist adherents are ideologically fused (exit is available and increasingly taken, but at generational and communal cost); laity are relationally bound to congregations; clergy are professionally fused through ordination investment. If the laity's relational frame broke, exit would spike faster than institutional barriers predict; if the literalist ideological frame broke, the parallel-institution ecosystem would contract sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: mainline_denominations collect credibility and institutional peace; academic_biblical_scholars convert scarce genre knowledge into interpretive authority; credentialed_clergy collect licensed teaching authority net of training costs; natural_scientific_community sits nearest the full-beneficiary end despite zero participation in the norm's administration — the reading removes scriptural counterclaims to natural history at no cost or effort to science. Victim declarations drive high directionality: plain_sense_lay_readers bear the deference transfer with constrained exit; literalist_tradition_adherents bear total in-community costs (standing, employment, platform), though their mobile exit to parallel institutions damps effective extraction below what trapped targets would register. Scientifically_informed_believers sit near symmetric: large coordination benefit, modest unverified-deference cost. No directionality overrides were needed: the beneficiary/victim declarations plus exit options already differentiate the seats, and a power-atom-keyed override would wrongly homogenize the three distinct organized seats (scholars, clergy, literalists).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — the post-Darwin forced choice between intellectual honesty and scriptural fidelity — remains live and is corroborated from outside the benefiting parties, so no mandatrophy resolution is declared. The hybrid classification guards against both mislabelings: calling this a pure coordination mechanism ignores the asymmetric transfer of interpretive jurisdiction upward and the real costs borne by the demoted literalist constituency through the same structure; calling it pure extraction ignores the genuine coordination dividend (faith-science coherence), the functional genre scholarship at its core, and the openness of alternatives outside adopting bodies. The theater ratio is treated as a symptom, not the test: the performative affirmation shell is real but the cost-asymmetry test is what distinguishes this from an inertial remnant — the administrators could revise the norm, but the institutional price of reverting to literalism (member defection, credibility collapse, faculty upheaval) exceeds what they bear under it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_binding_force_question,
    'Does the kernel''s authority survive non-literal instantiation, or does the theistic-evolution reading progressively hollow the textual authority it presumes to preserve?',
    'Longitudinal tracking of adopting communities: measure scripture-engagement rates, doctrinal retention across generations, and whether successive cohorts treat Genesis as binding revelation or as cultural inheritance.',
    'If the reading hollows the kernel, this constraint is a transitional stage drifting toward post-textual liberalism rather than a stable hybrid arrangement, and the network edge to the literalist sibling changes character from displacement to succession.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_binding_force_question, conceptual, 'Whether non-literal reading preserves or dissolves the textual authority it mediates.').

omega_variable(
    literalist_victim_status,
    'Are literalist tradition adherents victims of this reading''s operation inside adopting institutions, or merely external competitors whose parallel institutions leave them substantially unhurt?',
    'Compare career, standing, and community costs borne by literalists inside adopting denominations (exclusion from teaching posts, pulpit correction, seminary ineligibility, family division) against outcomes available in the parallel creationist institutional ecosystem.',
    'If costs are external-competitive only, the victim set contracts and the classification trends toward pure coordination; if in-community costs are heavy, the hybrid coordination/extraction reading is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literalist_victim_status, empirical, 'Whether the demoted literalist constituency bears costs through this structure or competes from outside it.').

omega_variable(
    expertise_or_capture_transfer,
    'Is the transfer of interpretive authority to credentialed specialists a genuine expertise premium or an elite-capture mechanism?',
    'Test whether lay readers equipped with open scholarly resources (original-language tools, critical commentaries, comparative Ancient Near Eastern texts) reach the non-literal conclusions independently; measure how often specialist mediation changes interpretive outcomes versus merely certifying them.',
    'If mediation adds little beyond gatekeeping, the deference component is extraction and epsilon rises; if it materially improves reading accuracy against the ancient context, it is coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(expertise_or_capture_transfer, empirical, 'Whether the specialist authority premium tracks real competence or positional gatekeeping.').

omega_variable(
    internalized_deference_ambiguity,
    'Is lay deference to specialist interpretation structurally enforced (credential gates, platform denial, curricular control) or internalized (trained interpretive humility that persists without enforcement)?',
    'Post-defection interviews: whether former members who come to reject the hermeneutic report lingering inability to trust their own reading of the text after leaving the enforcing institution.',
    'An internalized component raises effective suppression above the structural measure and predicts slower exit than institutional barriers alone would predict.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(internalized_deference_ambiguity, empirical, 'Structural versus internalized mechanism of lay interpretive deference.').

omega_variable(
    accommodation_integrity_tradeoff,
    'Should a community preserve plain textual fidelity at the cost of scientific estrangement, or purchase scientific peace at the cost of interpretive plainness?',
    'Not resolvable by data; turns on whether the community weights textual plainness or intellectual credibility as constitutive of fidelity to the text.',
    'A community weighting plainness would experience this reading as surrender rather than coordination, flipping the perceived beneficiary and victim structure entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(accommodation_integrity_tradeoff, preference, 'Values-level dispute over whether accommodation is fidelity or concession.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__theistic_evolution, 0, 165).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_cosmology__theistic_evolution, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t30, genesis_creation_cosmology__theistic_evolution, theater_ratio, 30, 0.14).
narrative_ontology:measurement_basis(gene_tr_t30, observed).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_cosmology__theistic_evolution, theater_ratio, 60, 0.2).
narrative_ontology:measurement_basis(gene_tr_t60, observed).
narrative_ontology:measurement(gene_tr_t90, genesis_creation_cosmology__theistic_evolution, theater_ratio, 90, 0.24).
narrative_ontology:measurement_basis(gene_tr_t90, observed).
narrative_ontology:measurement(gene_tr_t120, genesis_creation_cosmology__theistic_evolution, theater_ratio, 120, 0.28).
narrative_ontology:measurement_basis(gene_tr_t120, observed).
narrative_ontology:measurement(gene_tr_t150, genesis_creation_cosmology__theistic_evolution, theater_ratio, 150, 0.26).
narrative_ontology:measurement_basis(gene_tr_t150, observed).
narrative_ontology:measurement(gene_tr_t165, genesis_creation_cosmology__theistic_evolution, theater_ratio, 165, 0.25).
narrative_ontology:measurement_basis(gene_tr_t165, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t30, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 30, 0.22).
narrative_ontology:measurement_basis(gene_be_t30, observed).
narrative_ontology:measurement(gene_be_t60, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 60, 0.38).
narrative_ontology:measurement_basis(gene_be_t60, observed).
narrative_ontology:measurement(gene_be_t90, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 90, 0.45).
narrative_ontology:measurement_basis(gene_be_t90, observed).
narrative_ontology:measurement(gene_be_t120, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 120, 0.48).
narrative_ontology:measurement_basis(gene_be_t120, observed).
narrative_ontology:measurement(gene_be_t150, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 150, 0.44).
narrative_ontology:measurement_basis(gene_be_t150, observed).
narrative_ontology:measurement(gene_be_t165, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 165, 0.42).
narrative_ontology:measurement_basis(gene_be_t165, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 0, 0.08).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t30, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 30, 0.18).
narrative_ontology:measurement_basis(gene_su_t30, observed).
narrative_ontology:measurement(gene_su_t60, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 60, 0.42).
narrative_ontology:measurement_basis(gene_su_t60, observed).
narrative_ontology:measurement(gene_su_t90, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 90, 0.5).
narrative_ontology:measurement_basis(gene_su_t90, observed).
narrative_ontology:measurement(gene_su_t120, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 120, 0.52).
narrative_ontology:measurement_basis(gene_su_t120, observed).
narrative_ontology:measurement(gene_su_t150, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 150, 0.46).
narrative_ontology:measurement_basis(gene_su_t150, observed).
narrative_ontology:measurement(gene_su_t165, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 165, 0.4).
narrative_ontology:measurement_basis(gene_su_t165, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__theistic_evolution, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology__young_earth_literal).
narrative_ontology:affects_constraint(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology__literary_framework).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of the colloquial label 'what Genesis says about creation.' One kernel (binding textual authority over origins) decomposes into three structurally distinct constraints with different epsilon values, victim sets, and enforcement burdens: young_earth_literal (high textual jurisdiction, high science-conflict cost), theistic_evolution (this file — theological-domain-limited authority, moderate extraction via interpretive-authority transfer), and literary_framework (schema without cosmological claims). The upstream/downstream gradient runs from the literalist reading (historically prior, cited as the reading being displaced) to this one; the literary-framework sibling supplies much of this reading's scholarly warrant while differing on truth-content. Each member links the others via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
