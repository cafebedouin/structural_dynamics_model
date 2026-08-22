% ============================================================================
% CONSTRAINT STORY: genesis_creation_cosmology__theistic_evolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-10
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
    narrative_ontology:affects_constraint/2,
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
 *   human_readable: Non-Literal Hermeneutic Regime for the Genesis Creation Narratives (Theistic Evolution Reading)
 *   domain: religious/theological/philosophy_of_science
 *
 * SUMMARY:
 *   Within religious bodies that have accepted evolutionary cosmology, the
 *   Genesis creation narratives are read as conveying theological truth
 *   through non-literal literary forms — a hermeneutical regime that limits
 *   the text's authority to the theological domain and frees both the
 *   tradition and science from mutual veto. This story instantiates ONE
 *   reading of the kernel 'genesis_creation_cosmology': the
 *   theistic_evolution reading. The colloquial label 'what Genesis teaches
 *   about creation' covers three structurally distinct arrangements —
 *   young_earth_literal, theistic_evolution, and literary_framework — each
 *   with its own beneficiary structure, victim set, and epsilon; per the
 *   epsilon-invariance principle they are authored as separate stories in one
 *   constraint family, linked through network.affects_constraints. The
 *   claim/metric relationship here is deliberately unreconciled: the claimed
 *   type is what I judge structurally true of this reading's operation
 *   (genuine coordination with an enforced asymmetric cost structure), while
 *   the metrics describe what I judge descriptively true of how it actually
 *   runs — including real costs borne by literalist interpreters and lay
 *   literalists, which the reading's own expected structural delta concedes
 *   ('literalist doctrine enters victim set'). The epsilon referent is the
 *   standing arrangement under contest — the non-literal hermeneutical regime
 *   as instituted in adopting bodies — assessed by this reading's own lights,
 *   which acknowledge the displacement of literalists as a real cost rather
 *   than denying it.
 *
 * KEY AGENTS:
 *   - denominational_leadership: agenda-setting beneficiary (institutional power, arbitrage-grade exit) — sets ordination standards, curricula, and doctrinal statements; collects retention of scientifically educated members and allocates teaching office
 *   - professional_theologians: beneficiary (organized, mobile) — supply the mediating expertise the non-literal reading requires; their authority rises with the regime
 *   - scientifically_informed_laity: beneficiary (moderate, constrained) — retained in the tradition without being forced to reject mainstream science
 *   - scientific_community: incidental beneficiary (institutional, arbitrage) — relieved of scriptural veto inside major religious bodies
 *   - literalist_interpreters: primary payer (organized, constrained) — displaced from teaching office within adopting bodies; parallel institutions exist but at the price of schism
 *   - lay_literalist_congregants: payer (powerless, identity_locked) — bear delegitimation and conformity pressure; exit is fused with loss of faith itself
 *   - creationist_ministry_leaders: excluded (organized, trapped within this conversation) — run parallel institutions and oppose the regime from outside the rooms where it is set
 *   - religion_science_scholars: analytical observer — study the accommodation without confessional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_cosmology__theistic_evolution, 0.42).
domain_priors:suppression_score(genesis_creation_cosmology__theistic_evolution, 0.48).
domain_priors:theater_ratio(genesis_creation_cosmology__theistic_evolution, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, extractiveness, 0.42).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(genesis_creation_cosmology__theistic_evolution, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_cosmology__theistic_evolution, tangled_rope).
narrative_ontology:human_readable(genesis_creation_cosmology__theistic_evolution, "Non-Literal Hermeneutic Regime for the Genesis Creation Narratives (Theistic Evolution Reading)").
narrative_ontology:topic_domain(genesis_creation_cosmology__theistic_evolution, "religious/theological/philosophy_of_science").

domain_priors:requires_active_enforcement(genesis_creation_cosmology__theistic_evolution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_cosmology__theistic_evolution, '915e2b51-d8e8-4204-acc5-f76f3f987e73').
narrative_ontology:cs_kernel_codification('915e2b51-d8e8-4204-acc5-f76f3f987e73', fixed_text).
narrative_ontology:cs_authority_grounding('915e2b51-d8e8-4204-acc5-f76f3f987e73', lineage).
narrative_ontology:cs_interpretation_layer_present('915e2b51-d8e8-4204-acc5-f76f3f987e73').
narrative_ontology:cs_reading_relation('915e2b51-d8e8-4204-acc5-f76f3f987e73', genesis_creation_cosmology__young_earth_literal, forecloses).
narrative_ontology:cs_reading_relation('915e2b51-d8e8-4204-acc5-f76f3f987e73', genesis_creation_cosmology__literary_framework, coexists_with).
narrative_ontology:cs_axiom('915e2b51-d8e8-4204-acc5-f76f3f987e73', foundational, divine_accommodation_through_literary_forms).
narrative_ontology:cs_axiom_status(divine_accommodation_through_literary_forms, holdable).
narrative_ontology:cs_axiom_grounding('915e2b51-d8e8-4204-acc5-f76f3f987e73', divine_accommodation_through_literary_forms, theological).
narrative_ontology:cs_axiom('915e2b51-d8e8-4204-acc5-f76f3f987e73', foundational, compatibility_of_scripture_and_evolutionary_cosmology).
narrative_ontology:cs_axiom_status(compatibility_of_scripture_and_evolutionary_cosmology, holdable).
narrative_ontology:cs_axiom_grounding('915e2b51-d8e8-4204-acc5-f76f3f987e73', compatibility_of_scripture_and_evolutionary_cosmology, empirically_contingent).
narrative_ontology:cs_reference_frame('915e2b51-d8e8-4204-acc5-f76f3f987e73', inspired_theological_accommodation).
narrative_ontology:cs_drift_state('915e2b51-d8e8-4204-acc5-f76f3f987e73', contemporary_creation_science_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('915e2b51-d8e8-4204-acc5-f76f3f987e73', '').
narrative_ontology:cs_kernel_id(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, scientifically_informed_laity).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, professional_theologians).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, scientific_community).
narrative_ontology:constraint_beneficiary(genesis_creation_cosmology__theistic_evolution, denominational_leadership).
narrative_ontology:constraint_victim(genesis_creation_cosmology__theistic_evolution, literalist_interpreters).
narrative_ontology:constraint_victim(genesis_creation_cosmology__theistic_evolution, lay_literalist_congregants).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets ordination requirements, seminary curricula, and doctrinal statements for bodies that have adopted the non-literal reading of the creation narratives. Collects the retention of scientifically educated members who would otherwise leave, and allocates teaching office accordingly. Personal exit is easy — leaders move between denominations, academia, and parachurch roles — so enforcement choices are strategic rather than existential.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, denominational_leadership, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(genesis_creation_cosmology__theistic_evolution, denominational_leadership, beneficiary).

% Produce the commentaries, translations, and genre analyses that determine which passages count as figurative. Their training is the prerequisite for authoritative reading, so demand for their services rises with the non-literal approach. They can move between institutions and publish across the religious-secular divide.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, professional_theologians, beneficiary,
    organized, biographical, mobile, continental).

% Members educated in mainstream science who remain in the tradition because the non-literal reading removes the collision they would otherwise face. Leaving would cost them congregation, family ties, and inherited identity, so they accommodate to whatever interpretive line their body teaches.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, scientifically_informed_laity, beneficiary,
    moderate, biographical, constrained, global).

% Operates in a public environment where scriptural objections to geology, cosmology, and evolutionary biology carry reduced institutional weight inside major religious bodies. Its members depend on the arrangement not at all and can function identically whether or not it persists.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, scientific_community, beneficiary,
    institutional, generational, arbitrage, global).

% Pastors, teachers, and writers committed to reading the creation account as chronological report. Inside adopting bodies they are passed over for teaching posts, asked to sign statements they cannot sign, or steered toward parachurch work. Parallel creationist institutions exist, but reaching them means forfeiting standing, salary lines, and networks built inside the mainline.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, literalist_interpreters, payer,
    organized, biographical, constrained, national).

% Members whose assurance is bound up with taking the text at face value. Under the adopted reading their conviction is marked as naive or harmful; they face quiet pressure to conform, to stop asking, or to leave. Because their trust in the tradition is anchored to the text's plain sense, departure feels like losing the faith itself rather than changing pews.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, lay_literalist_congregants, payer,
    powerless, biographical, identity_locked, national).

% Run organizations outside the adopting bodies dedicated to restoring literal reading. They hold no seat in mainline doctrinal committees; their influence operates through publishing, media, and congregational pressure rather than through the rooms where the interpretive line is set.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, creationist_ministry_leaders, excluded,
    organized, biographical, trapped, national).

% Study the accommodation from outside confessional allegiance, tracing how the reading arose, what it costs its dissenters, and whether its exegetical foundations hold. They bear none of its costs and collect none of its benefits.
narrative_ontology:constraint_stakeholder(genesis_creation_cosmology__theistic_evolution, religion_science_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_cosmology__theistic_evolution, denominational_leadership).
narrative_ontology:fixing_cost_class(genesis_creation_cosmology__theistic_evolution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Resolves the collision between scriptural authority and scientific cosmology for believing communities: it defines a way to read the founding text that preserves both membership in the tradition and participation in mainstream science, so individuals and institutions need not choose between faith and education.
% TRANSFER_FUNCTION: Moves interpretive authority from plain-text readers to credentialed interpreters (theologians, biblical scholars, and the scientists whose findings set the interpretive agenda); moves literalist doctrine and its holders out of teaching office and doctrinal standing within adopting bodies; and moves the text's jurisdiction from natural phenomena to the theological domain alone.
% ABSENT_VOICES: Young-earth creationist ministries and lay literalists inside adopting denominations would object that the reading unbinds Scripture and concedes to fashionable science; they are absent from mainline doctrinal conversations — excluded from teaching posts, or organized in parallel institutions outside the room where the interpretive line is set.
% DISAPPEARANCE_RATIONALE: If the non-literal hermeneutic vanished overnight from adopting bodies, millions of scientifically educated believers would face a forced choice between faith and science; mainline denominations would bleed members or schism along the fault line; theistic-evolution institutions and their publishing ecosystems would dissolve; and science education in religious regions would lose one of its principal allies. The arrangements of every named seat depend on the regime's persistence.
% FOUNDING_PROBLEM: Built to solve the post-Darwin crisis: how a community committed to scriptural authority can survive universal scientific education when the plain sense of its founding text appears to contradict established cosmology and biology.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: historians of religion document the nineteenth- and twentieth-century crisis and the accommodation strategy in the mainstream seminaries; creationist ministries — the arrangement's adversaries — attest the problem is live by attacking the accommodation as capitulation; and science-education organizations independently document continuing faith-science conflict in religious communities. No seat inside the benefiting set is the sole attester.
narrative_ontology:disappearance_verdict(genesis_creation_cosmology__theistic_evolution, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_cosmology__theistic_evolution, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_cosmology__theistic_evolution, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
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
 *   Extractiveness is authored at 0.42: the regime delivers genuine goods (faith-science reconciliation, member retention, scientific legitimacy) while imposing real, concentrated costs on a minority — literalist interpreters lose standing and livelihood paths inside adopting bodies, and lay literalists face delegitimation of their core conviction. Suppression is authored at 0.48 as a raw structural property (unscaled by power or scope; only extractiveness is scaled by the engine): enforcement is communal and institutional rather than state-coercive — credential gates, required statements, pulpit allocation — and alternatives remain fully available outside adopting bodies, which caps suppression well below snare levels. Theater is low (0.18): the underlying exegetical scholarship is functional, with performative spikes during controversy waves (public statements of compatibility peak when the regime is attacked). Accessibility collapse is low (0.30): understanding the regime does not close off alternatives — literalist readings remain intellectually available and institutionally embodied elsewhere. Resistance is high (0.62): organized creationist movements, internal congregational pushback, and recurring seminary conflicts meet the regime continuously. The temporal series run on one shared six-point grid (every tracked metric authored at every examined year) and trace controversy waves rather than a designed reinforcement cycle: enforcement peaks track the fundamentalist-modernist controversy (1920s heresy trials, credential tests) and the creation-science resurgence (1980s-90s boundary hardening), with neo-evangelical detente between. The oscillation is driven by external adversarial mobilization, not by the arrangement itself, and the base_properties scalars reflect the 2025 endpoint of that shared grid.
 *
 * PERSPECTIVAL GAP:
 *   The payer seats and the administering/beneficiary seats should compute differently, and the structural data explains why. From the denominational-leadership and theologian seats, the regime is faithful scholarship that preserved the tradition through the scientific age — the coordination function is vivid and the costs invisible because they fall on people who have already been moved to the margins. From the literalist interpreter seat, the same regime is an enforced expulsion that redefines orthodoxy to exclude them. From the lay literalist seat, it is quieter and worse: their assurance is tied to the text's plain sense, so the regime does not merely cost them standing, it brands their faith-form as defective. The engine computes these divergent per-seat classifications from the power, exit, and directional data; the authored claim does not adjudicate between them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations map to low directionality: denominational_leadership (agenda-setter collecting retention and stability), professional_theologians (whose mediating role the regime makes indispensable), scientifically_informed_laity (net gainers, though with constrained exit keeping them from the arbitrage end), and scientific_community (incidental gainer with arbitrage-grade independence, sitting nearest the full-beneficiary end). Victim declarations map to high directionality: literalist_interpreters are organized but their exit is costly schism, placing them well toward the target end; lay_literalist_congregants combine powerlessness with identity-locked exit — their faith identity is fused with the text's plain sense — placing them nearest the full-target end, where effective extraction is amplified. Creationist_ministry_leaders hold no seat in the administering conversation (excluded), so they register as opposition rather than as a classified party. No directionality overrides are authored: the derivation chain from beneficiary/victim declarations plus power and exit atoms captures every seat's relationship without correction.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — surviving universal scientific education when the plain sense of the founding text appeared to contradict established cosmology — is still live, so no mandatrophy is declared and no zombie flag is expected from the status-times-verdict consumer (live founding problem paired with a world_rearranges verdict is the coherent cell). The classification guards against mislabeling in both directions: calling this a snare would erase the genuine coordination function (millions of believers are reconciled to science at real, non-confiscatory cost) and ignore that alternatives persist and exit is legally unobstructed; calling it a pure rope would erase the enforced asymmetry — the same institutional machinery that reconciles the majority actively displaces a named minority, and that displacement requires continuous enforcement (credential gates do not maintain themselves). The tangled_rope claim names exactly that hybrid: coordinated majority, paying minority, active enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contingency,
    'This constraint is one reading (theistic_evolution) of the kernel genesis_creation_cosmology; what would the sibling readings change structurally if adopted in place of it?',
    'Track denominational adoption patterns and doctrinal statements: under the young_earth_literal sibling the victim set inverts (scientific institutions and secular educators become the targeted parties and literalist interpreters become the administered beneficiaries); under the literary_framework sibling the theological-truth claim drops out and the arrangement narrows to a purely literary description with no doctrinal enforcement surface.',
    'The location of the victim set, the scope of textual authority, and therefore the computed extraction profile all shift with the reading adopted; classifications computed from this file are valid only for the theistic_evolution instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contingency, conceptual, 'Committer structure: one reading of a contested kernel; sibling readings instantiate different constraints.').

omega_variable(
    hermeneutic_durability_vs_fashion,
    'Is the non-literal reading a durable exegetical finding grounded in the text''s actual Ancient Near Eastern genre, or a defensive adaptation that tracks scientific fashion and would reverse if the scientific consensus reversed?',
    'Comparative philology and reception history: if the genre case stands independent of scientific pressure (as the pre-Darwin accommodationist strands suggest), the reading is text-grounded; if adoption rates track scientific prestige rather than exegetical argument, it is fashion-driven.',
    'If fashion-driven, the arrangement''s coordination function is contingent and its classification carries scaffold-like transience risk; if text-grounded, the coordination function is stable and the tangled_rope reading is robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutic_durability_vs_fashion, empirical, 'Whether the hermeneutic''s foundation is exegetical discovery or adaptive concession.').

omega_variable(
    literalist_victim_status,
    'Are lay literalists inside adopting bodies genuinely bearing imposed costs, or are they self-selected dissenters whose losses follow from their own refusal to conform?',
    'Examine sanction records: credential denials, pulpit exclusions, and required statement signings establish imposition; purely voluntary departures without institutional barrier would establish self-selection.',
    'If dissenters-by-choice dominate, the extraction asymmetry weakens and the arrangement trends toward pure coordination; if sanctioned dissent dominates, the enforced-asymmetry component strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(literalist_victim_status, conceptual, 'Whether the payer seat reflects imposed cost or chosen dissent.').

omega_variable(
    mediation_rent_or_service,
    'Does the requirement that ordinary readers accept expert-mediated interpretation constitute a clerical interpretive monopoly, or a genuine service rendered by texts that are genuinely hard?',
    'Test accessibility directly: distribute well-designed lay-facing guides to the genre argument and measure whether independent lay interpretation converges on expert conclusions; convergence indicates service, persistent divergence indicates gatekeeping.',
    'If gatekeeping, a measurable share of the arrangement''s cost to lay readers is positional rent accruing to the interpreter class; if service, that share is legitimate coordination cost.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mediation_rent_or_service, empirical, 'Whether interpretive mediation extracts rents or delivers real interpretive labor.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the suppression experienced by literalist-leaning members structural (credential gates, hiring barriers, pulpit access rules) or internalized (identity fusion in which the text''s plain sense and the faith itself are fused, so exit feels like apostasy)?',
    'Post-exit suppression trajectory: interview members who left adopting bodies for literalist ones; if conformity pressure and self-deprecation persist after the structural barrier is removed, a substantial internalized component is established.',
    'If substantially internalized, the arrangement''s effective hold on the payer seat exceeds what its institutional machinery alone explains, and exit-option assessments for lay literalists should weight identity lock more heavily.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism in the payer population.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_cosmology__theistic_evolution, 1859, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1859, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1859, 0.1).
narrative_ontology:measurement(gene_tr_t1900, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1900, 0.14).
narrative_ontology:measurement(gene_tr_t1925, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1925, 0.22).
narrative_ontology:measurement(gene_tr_t1955, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1955, 0.16).
narrative_ontology:measurement(gene_tr_t1990, genesis_creation_cosmology__theistic_evolution, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(gene_tr_t2025, genesis_creation_cosmology__theistic_evolution, theater_ratio, 2025, 0.18).

% Extraction over time
narrative_ontology:measurement(gene_be_t1859, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1859, 0.28).
narrative_ontology:measurement(gene_be_t1900, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1900, 0.38).
narrative_ontology:measurement(gene_be_t1925, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1925, 0.52).
narrative_ontology:measurement(gene_be_t1955, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1955, 0.44).
narrative_ontology:measurement(gene_be_t1990, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 1990, 0.5).
narrative_ontology:measurement(gene_be_t2025, genesis_creation_cosmology__theistic_evolution, base_extractiveness, 2025, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1859, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1859, 0.2).
narrative_ontology:measurement(gene_su_t1900, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1900, 0.35).
narrative_ontology:measurement(gene_su_t1925, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1925, 0.6).
narrative_ontology:measurement(gene_su_t1955, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1955, 0.45).
narrative_ontology:measurement(gene_su_t1990, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 1990, 0.55).
narrative_ontology:measurement(gene_su_t2025, genesis_creation_cosmology__theistic_evolution, suppression_requirement, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_cosmology__theistic_evolution, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology__young_earth_literal).
narrative_ontology:affects_constraint(genesis_creation_cosmology__theistic_evolution, genesis_creation_cosmology__literary_framework).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'what Genesis teaches about creation' conflates three structurally distinct arrangements. Young_earth_literal (literal six-day recent creation) assigns the text maximal cosmological jurisdiction and inverts this story's beneficiary/victim structure. Literary_framework (ANE schema as literary device without cosmological claim) strips the theological-truth assertion and with it the enforcement surface. Theistic_evolution (this story) occupies the middle: theological truth through non-literal forms, textual authority limited to the theological domain, literalist doctrine entering the victim set. Each story carries its own epsilon, stakeholders, and claimed type; the upstream empirical claim (deep-time cosmology) is cited by this reading as grounds for its hermeneutic, which is why the network edge runs from the scientific side into this story and from this story toward its siblings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
