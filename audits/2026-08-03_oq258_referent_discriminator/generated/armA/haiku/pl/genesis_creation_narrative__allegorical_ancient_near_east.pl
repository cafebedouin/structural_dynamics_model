% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__allegorical_ancient_near_east
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__allegorical_ancient_near_east, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: genesis_creation_narrative__allegorical_ancient_near_east
 *   human_readable: Genesis 1-2 as Ancient Near Eastern Mythopoetic Cosmogony (Allegorical Reading)
 *   domain: religious_studies/biblical_hermeneutics/science_religion_interface
 *
 * SUMMARY:
 *   Genesis 1-2 is a contested interpretive site where three major readings
 *   compete for institutional legitimacy: the allegorical-ANE reading
 *   (Genesis as ancient Mesopotamian-influenced mythopoesis, theologically
 *   authoritative but cosmologically non-referential); the
 *   literal-young-earth reading (Genesis as inerrant historical-scientific
 *   chronicle, 24-hour creation days, recent earth); and theistic evolution
 *   (Genesis as theological framework compatible with evolutionary cosmology,
 *   days as symbolic or epochs). This constraint is the allegorical-ANE
 *   reading instantiated as a commitment system story. The reading emerged in
 *   19th-century critical scholarship as a solution to an apparent dilemma:
 *   how to maintain biblical theological authority while adopting
 *   historical-critical method. Under this reading, Genesis 1-2 have zero
 *   adjudicative force over cosmology or evolutionary biology; their
 *   authority is entirely theological and literary-pedagogical. The dominion
 *   mandate (1:28-30) loses its role as a normative environmental ethic and
 *   becomes instead a mythic charter requiring separate reasoning. This
 *   reading operates with moderate extraction: it benefits scholars and
 *   science educators at some cost to literalist-evangelical communities. The
 *   cost is not physical coercion but hermeneutical displacement—one's
 *   interpretive practice becomes redefined as naive or culturally
 *   conditioned rather than merely different.
 *
 * KEY AGENTS:
 *   - Critical biblical scholars — benefit from institutional legitimacy of historical-critical method
 *   - Science education advocates — benefit from decoupling Genesis from cosmology in policy contexts
 *   - Literalist evangelical tradition — identity-locked payers bearing hermeneutical displacement cost
 *   - Young-earth creationism organizations — suffer institutional legitimacy loss and research program undermining
 *   - Theistic evolutionary theologians — intermediate observers with partial alignment
 *   - Religious communities reading Genesis devotionally — excluded from academic verdict but affected by pulpit consequences
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__allegorical_ancient_near_east, 0.31).
domain_priors:suppression_score(genesis_creation_narrative__allegorical_ancient_near_east, 0.28).
domain_priors:theater_ratio(genesis_creation_narrative__allegorical_ancient_near_east, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, extractiveness, 0.31).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, accessibility_collapse, 0.42).
narrative_ontology:constraint_metric(genesis_creation_narrative__allegorical_ancient_near_east, resistance, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__allegorical_ancient_near_east, rope).
narrative_ontology:human_readable(genesis_creation_narrative__allegorical_ancient_near_east, "Genesis 1-2 as Ancient Near Eastern Mythopoetic Cosmogony (Allegorical Reading)").
narrative_ontology:topic_domain(genesis_creation_narrative__allegorical_ancient_near_east, "religious_studies/biblical_hermeneutics/science_religion_interface").

domain_priors:requires_active_enforcement(genesis_creation_narrative__allegorical_ancient_near_east).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__allegorical_ancient_near_east, '32153b4c-7af2-41ce-a6cc-0556f46fafaa').
narrative_ontology:cs_kernel_codification('32153b4c-7af2-41ce-a6cc-0556f46fafaa', fixed_text).
narrative_ontology:cs_authority_grounding('32153b4c-7af2-41ce-a6cc-0556f46fafaa', lineage).
narrative_ontology:cs_interpretation_layer_present('32153b4c-7af2-41ce-a6cc-0556f46fafaa').
narrative_ontology:cs_reading_relation('32153b4c-7af2-41ce-a6cc-0556f46fafaa', genesis_creation_narrative__literal_young_earth, coexists_with).
narrative_ontology:cs_reading_relation('32153b4c-7af2-41ce-a6cc-0556f46fafaa', genesis_creation_narrative__theistic_evolutionary, coexists_with).
narrative_ontology:cs_axiom('32153b4c-7af2-41ce-a6cc-0556f46fafaa', foundational, genesis_mythopoetic_not_cosmological).
narrative_ontology:cs_axiom_status(genesis_mythopoetic_not_cosmological, holdable).
narrative_ontology:cs_axiom_grounding('32153b4c-7af2-41ce-a6cc-0556f46fafaa', genesis_mythopoetic_not_cosmological, empirically_contingent).
narrative_ontology:cs_axiom('32153b4c-7af2-41ce-a6cc-0556f46fafaa', foundational, theological_authority_independent_of_empirical_claim).
narrative_ontology:cs_axiom_status(theological_authority_independent_of_empirical_claim, holdable).
narrative_ontology:cs_axiom_grounding('32153b4c-7af2-41ce-a6cc-0556f46fafaa', theological_authority_independent_of_empirical_claim, deontological).
narrative_ontology:cs_reference_frame('32153b4c-7af2-41ce-a6cc-0556f46fafaa', critical_hermeneutical_method).
narrative_ontology:cs_drift_state('32153b4c-7af2-41ce-a6cc-0556f46fafaa', contemporary_science_theology_discourse, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('32153b4c-7af2-41ce-a6cc-0556f46fafaa', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__allegorical_ancient_near_east, science_education_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, literalist_evangelical_tradition).
narrative_ontology:constraint_victim(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_creationism_organizations).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, form_critical_methodology).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, ancient_near_eastern_literary_context).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__allegorical_ancient_near_east, theological_intent_vs_cosmological_claim).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Scholars trained in source criticism, form criticism, and Ancient Near Eastern textual analysis benefit from a reading that licenses their interpretive tools as the primary lens. This reading provides institutional legitimacy for academic biblical hermeneutics and creates career pathways in historical-critical exegesis. Their exit is toward fundamentalist seminaries or apologetics—a genuine option but one requiring identity reorientation.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars, beneficiary,
    organized, generational, mobile, global).

% Benefit from a reading that decouples theological authority from cosmological claims, eliminating the 'Genesis vs. Evolution' jurisdictional conflict in public schools and science curricula. This reading removes the primary basis for creationism advocacy in policy contexts and strengthens the institutional boundary between science and theology. Their exit is upward to secular naturalism—a move away rather than a loss.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, science_education_advocates, beneficiary,
    powerful, generational, arbitrage, national).

% Bears the cost of this reading through hermeneutical displacement: the literal meaning they hold as foundational to faith authority becomes reinterpreted as peripheral mythopoesis. Their interpretive practice (straightforward reading of text as historical-scientific fact) is redefined as naive or culturally conditioned. They retain institutional autonomy but at cost to their claims of universal textual authority. Exit requires reconstructing how they understand revelation itself.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, literalist_evangelical_tradition, payer,
    organized, generational, identity_locked, global).

% Organizations built on the premise that Genesis 1-2 contains scientific information suffer institutional legitimacy loss under this reading. Research programs, educational materials, and policy advocacy are undermined by the constraint's operation—the reading reframes their empirical claims as category mistakes. They can continue operating but face mounting institutional isolation from secular academia and mainline theology.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, young_earth_creationism_organizations, payer,
    moderate, biographical, constrained, global).

% Occupy an intermediate position: this reading enables their framework (Genesis as theology-only) while their framework claims to honor Genesis's own intent more respectfully than literalism. They can adopt this reading and remain distinct, or resist it by insisting science-theology compatibility requires more textual content.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, theistic_evolutionary_theologians, observer,
    organized, generational, mobile, global).

% Congregational and devotional communities worldwide read Genesis for spiritual formation and cosmological orientation—they are not included in the scholarly debate but are affected by which reading becomes institutionally dominant. Their alternative reading (Genesis as received theological truth, textual authority) has voices but no seat at academic hermeneutical councils.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__allegorical_ancient_near_east, religious_communities_reading_genesis, excluded,
    moderate, biographical, constrained, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__allegorical_ancient_near_east, critical_biblical_scholars).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__allegorical_ancient_near_east, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the scholarly coordination problem: how to read Genesis in light of historical-critical method, ANE parallels (Enuma Elish, Atrahasis cosmogonies), and modern cosmology without loss of theological authority. Provides a coherent framework for biblical scholars to operate within both academic and theological communities without methodological contradiction.
% TRANSFER_FUNCTION: Moves interpretive authority from literalist/plain-reading communities toward academic-historical-critical readers; shifts the meaning of 'Genesis authority' from cosmological-historical to theological-pedagogical; reframes the dominion mandate (Genesis 1:28-30) from a normative environmental ethic to a mythic charter that requires separate ethical reasoning.
% ABSENT_VOICES: Congregational communities and devotional readers who rely on Genesis for spiritual cosmology are structurally excluded from the hermeneutical debate. They have no voice in academic biblical studies councils but experience the constraint's effects through pulpit exegesis and religious education shaped by scholarly consensus. Young-earth creationism organizations also have constrained voice—their empirical claims are redefined as out-of-category rather than engaged scientifically.
% DISAPPEARANCE_RATIONALE: If this reading disappeared, the science-religion conflict would re-intensify around Genesis; literalism would reassert cosmological claims requiring active rebuttal in public education; the scholarly hermeneutical landscape would reorganize around different interpretive authority bases. But theological reading of Genesis itself would persist, whether through literalism or theistic evolution—so the constraint does not sustain Genesis reading, only a particular reading's institutional legitimacy.
% FOUNDING_PROBLEM: Early critical biblical scholarship (19th-20th centuries) faced a genuine coordination crisis: how to maintain both historical-critical method and theological authority when the two appeared to demand contradictory readings of the same text. The literalist reading foreclosed critical method; ignoring the text's theological claim foreclosed theology. The allegorical-ANE reading was developed to solve this: Genesis is theologically authoritative precisely as ANE mythopoesis, not despite it.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by scholars like Brevard Childs and canonical criticism advocates (theological reading), and by form-critical pioneers like Hermann Gunkel (historical-critical reading). But the problem's ONGOING status is contested: defenders of the reading argue the coordination crisis persists (Genesis in schools, faith/science discourse); literalists argue the problem was artificially created by abandoning textual authority; theistic evolutionists argue their framework resolves it better. No corroboration from outside the benefiting/paying parties—the scientific establishment is silent on how Genesis should be read theologically, and congregational communities are excluded from the scholarly verdict.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__allegorical_ancient_near_east, contested).
narrative_ontology:founding_problem_status(genesis_creation_narrative__allegorical_ancient_near_east, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__allegorical_ancient_near_east, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-04',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(genesis_creation_narrative__allegorical_ancient_near_east, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__allegorical_ancient_near_east, 0.31, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).
:- end_tests(genesis_creation_narrative__allegorical_ancient_near_east_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.31 end-state, climbing from 0.18 at interval start). The extraction is not direct material rent but interpretive authority: the reading transfers who gets to set the meaning of Genesis in institutional contexts (academia, seminaries, textbooks). The suppression requirement (0.28) is moderate because the reading's persistence depends on active gatekeeping in academic hiring, publishing, and curriculum (exclusion of literalist alternatives from mainstream biblical studies journals and seminaries) rather than on incentive realignment. Theater is low (0.18) because the reading does genuine hermeneutical work (ANE parallels are real, form criticism has substance), not mere ceremonial performance. The accessibility_collapse is moderate (0.42) because alternatives remain available—literalist readings persist in evangelical seminaries and grassroots communities—but the cost of holding them in academic contexts has risen substantially. Resistance is high (0.68) because literalist and young-earth communities actively resist this reading, not passively accepting it. The measurement series shows extractiveness and suppression_requirement both rising sharply in the early interval (0-24, t0-t24), plateauing around t32-t40 where the reading achieves institutional entrenchment, then staying flat. Theater ratio rises more slowly, indicating the reading's functional content persists even as institutional entrenchment increases. This plateau pattern is diagnostic: the constraint solved the original coordination problem (19th-century critical method vs. theological authority) and now maintains itself through institutional inertia more than through active problem-solving.
 *
 * PERSPECTIVAL GAP:
 *   The perspective divergence is acute. From the critical-scholar seat, this reading is genuine intellectual liberation: it solves the false dilemma between faith and reason by reclassifying the problem's premises. From the literalist seat, it is epistemic displacement: one's textual interpretive practice becomes redefined as unsophisticated. From the science educator's seat, it is institutional unblocking: the text is moved out of the science curriculum conflict. From the young-earth organization's seat, it is institutional isolation: their research and advocacy are rendered category-mistakes rather than empirical claims. From the theistic-evolution seat, it is a partial threat: this reading enables their framework while implicitly suggesting their framework over-accommodates science and under-respects the text. From excluded congregational communities, it is irrelevance: their spiritual reading of the text has no voice in what counts as 'proper' interpretation. The engine will compute these divergences from the stakeholder power/exit_options/role structures, showing a range of classifications from rope (beneficial coordination for scholars, resistance overcome) to snare (extraction with limited alternative for literalists) depending on the seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Critical biblical scholars sit near d=0.0 (full beneficiary): they collect interpretive authority and institutional legitimacy from this reading, they have mobile exit (can move to history of religions, theology, literature), and they retain powerful organized position. Science educators sit near d=0.15-0.25 (beneficiary with modest cost): they benefit institutionally but also depend on the reading's coherence with multiple constituencies. Literalist-evangelical communities sit near d=0.75-0.85 (near-full target): they bear identity costs, constrained exit (leaving literalism requires reconstructing their understanding of revelation), and their interpretive practice is displaced without direct say in the outcome. YEC organizations sit similarly. Theistic evolutionists sit near d=0.5 (symmetric or slightly beneficiary, ~0.35-0.45): they are not primary beneficiaries but the reading enables their framework; they have no major exit barriers. The directionality derivation runs primarily on the beneficiary/victim declaration plus exit_options: beneficiaries (critical scholars, science advocates) with mobile/arbitrage exit produce low d; victims (literalists, YEC) with identity_locked exit produce high d. No directionality_overrides are needed because the structural derivation captures the asymmetry accurately.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy (mandate outliving its function) is a partial fit. The founding problem—how to hold both critical method and theological authority—is CONTESTED in status. The reading solved it, and the solution persists. But the problem's LIVENESS is disputed: critical scholars say the problem is still live (Genesis still evokes literal claims in congregations, still generates science-conflict in schools); literalists say the problem was artificially created by abandoning textual authority; theistic evolutionists say their framework solves it better. The constraint's persistence is not due to forgotten function (piton-type) but due to unresolved dispute about whether the founding function is still needed. This fits mandatrophy's symptom (a reading that persists despite question about its necessity) but not its full diagnosis (a reading whose mandate is clearly dead). The theater_ratio (0.18) is relatively low, suggesting the reading is not pure performance—it still does hermeneutical work. A true piton would show theater_ratio > 0.5, with the reading maintained largely through institutional ritual while actual scholarly practice quietly diverges. This reading's theater is higher than that because scholars genuinely employ ANE comparison and form-critical tools—the reading is not yet a theater piece. However, the plateau in both extractiveness and theater around t32-t40 suggests early signs of inertial maintenance: the reading no longer solves coordination problems as actively as it did but persists through institutional gatekeeping. This is piton-precursor territory, not piton proper.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    hermeneutical_reading_contest,
    'Is the allegorical-ANE reading a genuine coordinate solution to a real scholarly problem (science-theology conflict), or a cover story for subordinating literalist communities to academic authority?',
    'Track whether congregational and literalist communities experience the reading as liberation from false dilemmas or as epistemic displacement. Post-adoption surveys of communities who adopt this reading vs. those who resist; study whether adoption correlates with voluntary interpretive change or institutional pressure (funding, accreditation, educational gatekeeping).',
    'If genuine coordination: the constraint is rightly classified as rope with real beneficiaries and legitimate costs. If cover story: the extraction is higher than authored (0.31 becomes 0.55+) and the constraint reclassifies toward snare; suppression becomes the primary mechanism, not coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(hermeneutical_reading_contest, empirical, 'Whether the reading solves a real coordination problem or masks asymmetric epistemic authority.').

omega_variable(
    commission_vs_reinterpretation_ambiguity,
    'Does the reading''s claim that Genesis is ''theological, not cosmological'' change the actual text''s meaning, or merely the permission structure for how it can be read in academic contexts?',
    'Textual-historical analysis: trace whether ANE scholarship genuinely discovered this distinction in Genesis (the text was always ANE-embedded) or whether the distinction was imported by modern hermeneutical needs. Compare pre-modern Jewish and Christian readings: did they distinguish theological from cosmological claims?',
    'If the distinction is authentic to the text''s historical embedding: the reading is interpretive recovery (higher coordination legitimacy). If the distinction is modern (imposed by 20th-century disciplinary boundaries): the reading is hermeneutical innovation that creates a new interpretive grid, raising questions about whether it respects the text''s own authority claims.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commission_vs_reinterpretation_ambiguity, conceptual, 'Whether the theological-cosmological distinction is intrinsic to the text or a modern hermeneutical imposition.').

omega_variable(
    identity_lock_mechanism_for_literalists,
    'For literalist communities, how much of their exit resistance is structural constraint (limited education/network/career paths outside literalism) vs. identity-fusion (literalism is constitutive of their understanding of revelation, faith, tradition)?',
    'Post-exit trajectory study: where literalist scholars or pastors who adopt the allegorical reading describe their transition, track whether they experienced loss of identity or loss of epistemic permission. Separate structural from internalized suppression.',
    'If structural: the constraint''s suppression (0.28) is primarily external gatekeeping; targeted institutional change could lower it significantly. If identity-fused: the suppression is internalized; even institutional opening leaves the cost in place for those deeply embedded in literalist identity. The mechanism affects both the measured suppression accuracy and the feasibility of remedies.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_for_literalists, empirical, 'The structure of exit costs for literalist readers under this reading.').

omega_variable(
    sibling_reading_foreclosure,
    'Does the allegorical-ANE reading logically foreclose the literal-young-earth reading within a single commitment framework, or do they represent genuinely coexisting frameworks held by different parties?',
    'Examine whether a single reader can hold both (Genesis is mythologically ancient-ANE AND literarily true as 24-hour creation) without internal contradiction. Test among theistic evolutionists and scholars who shift between readings.',
    'If foreclosed: the relation should be ''forecloses'' in cs_structure.reading_relations. If coexisting: the relation is ''coexists_with''. The answer shapes whether this reading''s institutional dominance necessarily excludes literalism or merely marginalizes it while allowing parallel tradition.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_foreclosure, conceptual, 'The logical relationship between the allegorical and literalist readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__allegorical_ancient_near_east, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 0, 0.08).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t8, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 8, 0.12).
narrative_ontology:measurement_basis(gene_tr_t8, observed).
narrative_ontology:measurement(gene_tr_t16, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 16, 0.15).
narrative_ontology:measurement_basis(gene_tr_t16, observed).
narrative_ontology:measurement(gene_tr_t24, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 24, 0.17).
narrative_ontology:measurement_basis(gene_tr_t24, observed).
narrative_ontology:measurement(gene_tr_t32, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 32, 0.18).
narrative_ontology:measurement_basis(gene_tr_t32, observed).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(gene_tr_t40, observed).
narrative_ontology:measurement(gene_tr_t50, genesis_creation_narrative__allegorical_ancient_near_east, theater_ratio, 50, 0.18).
narrative_ontology:measurement_basis(gene_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 0, 0.18).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t8, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 8, 0.24).
narrative_ontology:measurement_basis(gene_be_t8, observed).
narrative_ontology:measurement(gene_be_t16, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 16, 0.28).
narrative_ontology:measurement_basis(gene_be_t16, observed).
narrative_ontology:measurement(gene_be_t24, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 24, 0.3).
narrative_ontology:measurement_basis(gene_be_t24, observed).
narrative_ontology:measurement(gene_be_t32, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 32, 0.31).
narrative_ontology:measurement_basis(gene_be_t32, observed).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 40, 0.31).
narrative_ontology:measurement_basis(gene_be_t40, observed).
narrative_ontology:measurement(gene_be_t50, genesis_creation_narrative__allegorical_ancient_near_east, base_extractiveness, 50, 0.31).
narrative_ontology:measurement_basis(gene_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t8, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 8, 0.2).
narrative_ontology:measurement_basis(gene_su_t8, observed).
narrative_ontology:measurement(gene_su_t16, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 16, 0.24).
narrative_ontology:measurement_basis(gene_su_t16, observed).
narrative_ontology:measurement(gene_su_t24, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 24, 0.27).
narrative_ontology:measurement_basis(gene_su_t24, observed).
narrative_ontology:measurement(gene_su_t32, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 32, 0.28).
narrative_ontology:measurement_basis(gene_su_t32, observed).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 40, 0.28).
narrative_ontology:measurement_basis(gene_su_t40, observed).
narrative_ontology:measurement(gene_su_t50, genesis_creation_narrative__allegorical_ancient_near_east, suppression_requirement, 50, 0.28).
narrative_ontology:measurement_basis(gene_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__allegorical_ancient_near_east, information_standard).
narrative_ontology:boltzmann_floor_override(genesis_creation_narrative__allegorical_ancient_near_east, 0.05).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__literal_young_earth).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, genesis_creation_narrative__theistic_evolutionary).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, creationism_in_public_schools).
narrative_ontology:affects_constraint(genesis_creation_narrative__allegorical_ancient_near_east, science_biology_curriculum_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Genesis creation narrative kernel. The allegorical-ANE reading decouples the text from cosmological authority entirely, affecting downstream constraints about science curriculum and creationism policy. The literal-young-earth reading (separate story) will maintain cosmological authority, producing different extraction patterns and resistance. The theistic-evolutionary reading (separate story) will attempt synthesis, showing how a middle position experiences pressure from both extremes. All three stories share the same textual kernel but author different ε values, beneficiary/victim structures, and institutional effects. Network edges link all three via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
