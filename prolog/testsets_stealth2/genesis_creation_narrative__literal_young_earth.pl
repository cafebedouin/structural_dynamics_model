% ============================================================================
% CONSTRAINT STORY: genesis_creation_narrative__literal_young_earth
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_genesis_creation_narrative__literal_young_earth, []).

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
 *   constraint_id: genesis_creation_narrative__literal_young_earth
 *   human_readable: Enforced Literalist Reading of Genesis 1-2 as Inerrant Historical-Scientific Chronicle
 *   domain: religious/hermeneutical/educational
 *
 * SUMMARY:
 *   Within conservative Protestant institutions, Genesis 1-2 is required
 *   reading as inerrant historical-scientific chronicle: six twenty-four-hour
 *   days, a recent creation, a historical Adam, and downstream from these,
 *   the categorical falsity of biological evolution. The requirement is
 *   administered through doctrinal statements, employment covenants,
 *   curricula, and social discipline. This story instantiates ONE reading of
 *   the genesis_creation_narrative kernel (literal_young_earth); the
 *   theistic-evolutionary and ancient-Near-Eastern-allegorical readings are
 *   separate constraints with their own epsilon values, linked in the
 *   network. The claim/metric gap is deliberate: the reading presents itself
 *   as simple fidelity to the text's plain sense (rope-like from its own
 *   seat), while the authored metrics describe actively enforced,
 *   substantially extractive operation - the engine measures that divergence.
 *   Epsilon's referent is the standing enforced-literalist arrangement
 *   itself, not any sibling reading's endorsed alternative. KEY AGENTS (by
 *   structural relationship): inerrantist_institutional_leadership
 *   (agenda-setter, institutional/constrained);
 *   creation_apologetics_ministries (primary beneficiary,
 *   organized/arbitrage); christian_college_science_faculty (target,
 *   moderate/trapped); doubting_lay_members (target,
 *   powerless/identity_locked); creationist_schooled_students (target with
 *   incidental benefits, powerless/trapped); nonliteralist_scholars (excluded
 *   voice, organized/mobile); mainstream_scientific_community (analytical
 *   observer, institutional/analytical).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(genesis_creation_narrative__literal_young_earth, 0.7).
domain_priors:suppression_score(genesis_creation_narrative__literal_young_earth, 0.8).
domain_priors:theater_ratio(genesis_creation_narrative__literal_young_earth, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, extractiveness, 0.7).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(genesis_creation_narrative__literal_young_earth, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(genesis_creation_narrative__literal_young_earth, tangled_rope).
narrative_ontology:human_readable(genesis_creation_narrative__literal_young_earth, "Enforced Literalist Reading of Genesis 1-2 as Inerrant Historical-Scientific Chronicle").
narrative_ontology:topic_domain(genesis_creation_narrative__literal_young_earth, "religious/hermeneutical/educational").

domain_priors:requires_active_enforcement(genesis_creation_narrative__literal_young_earth).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(genesis_creation_narrative__literal_young_earth, '927aacb1-e40e-49c7-a36d-463a667942c9').
narrative_ontology:cs_kernel_codification('927aacb1-e40e-49c7-a36d-463a667942c9', fixed_text).
narrative_ontology:cs_authority_grounding('927aacb1-e40e-49c7-a36d-463a667942c9', lineage).
narrative_ontology:cs_interpretation_layer_present('927aacb1-e40e-49c7-a36d-463a667942c9').
narrative_ontology:cs_reading_relation('927aacb1-e40e-49c7-a36d-463a667942c9', genesis_creation_narrative__theistic_evolutionary, forecloses).
narrative_ontology:cs_reading_relation('927aacb1-e40e-49c7-a36d-463a667942c9', genesis_creation_narrative__allegorical_ancient_near_east, forecloses).
narrative_ontology:cs_axiom('927aacb1-e40e-49c7-a36d-463a667942c9', foundational, inerrancy_requires_plain_sense_history).
narrative_ontology:cs_axiom_status(inerrancy_requires_plain_sense_history, holdable).
narrative_ontology:cs_axiom_grounding('927aacb1-e40e-49c7-a36d-463a667942c9', inerrancy_requires_plain_sense_history, deontological).
narrative_ontology:cs_axiom('927aacb1-e40e-49c7-a36d-463a667942c9', foundational, sound_science_confirms_recent_creation).
narrative_ontology:cs_axiom_status(sound_science_confirms_recent_creation, holdable).
narrative_ontology:cs_axiom_grounding('927aacb1-e40e-49c7-a36d-463a667942c9', sound_science_confirms_recent_creation, empirically_contingent).
narrative_ontology:cs_axiom('927aacb1-e40e-49c7-a36d-463a667942c9', secondary, pre_fall_animal_death_theologically_impermissible).
narrative_ontology:cs_axiom_status(pre_fall_animal_death_theologically_impermissible, holdable).
narrative_ontology:cs_axiom_grounding('927aacb1-e40e-49c7-a36d-463a667942c9', pre_fall_animal_death_theologically_impermissible, theological).
narrative_ontology:cs_reference_frame('927aacb1-e40e-49c7-a36d-463a667942c9', plain_sense_inerrant_chronicle).
narrative_ontology:cs_drift_state('927aacb1-e40e-49c7-a36d-463a667942c9', contemporary_geological_genomic_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('927aacb1-e40e-49c7-a36d-463a667942c9', '').
narrative_ontology:cs_kernel_id(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, inerrantist_institutional_leadership).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, creation_apologetics_ministries).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, christian_college_science_faculty).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, doubting_lay_members).
narrative_ontology:constraint_victim(genesis_creation_narrative__literal_young_earth, creationist_schooled_students).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(genesis_creation_narrative__literal_young_earth, creationist_schooled_students).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__literal_young_earth, biblical_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__literal_young_earth, historical_adam_necessity).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__literal_young_earth, recent_creation_chronology).
narrative_ontology:constraint_vindicates(genesis_creation_narrative__literal_young_earth, global_flood_geology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Seminary presidents, denominational boards, and college trustees who write and enforce doctrinal statements requiring affirmation of Genesis 1-2 as literal history. They hire only signers of the covenant, discipline or remove teachers who deviate, and publish position papers marking the boundaries. Their own mobility is limited: their authority, livelihoods, and life work are invested in the institutions whose identity the requirement anchors, so softening the requirement threatens everything they administer.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, inerrantist_institutional_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Organizations producing books, school curricula, museums, conferences, and media defending recent creation. Every institution that mandates the literal reading generates demand for their teaching materials, speaking events, and reassurance products. They operate across media platforms and jurisdictions and can move audiences and revenue streams with little friction.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, creation_apologetics_ministries, beneficiary,
    organized, biographical, arbitrage, global).

% Biologists, geologists, and astronomers teaching at colleges that require signed covenants affirming recent creation. Many trained at secular graduate schools and know the state of the evidence firsthand. Signing requires public affirmation they may privately doubt; open dissent risks termination, loss of standing in the evangelical academy, and loss of vocation. Leaving means abandoning career and religious community at the same time.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, christian_college_science_faculty, payer,
    moderate, biographical, trapped, national).

% Congregants in communities where the literal reading is preached as the marker of real faith. Questions about the age of the earth or evolution are treated as spiritual danger, and voicing them risks being labeled liberal or unbelieving. Friendships, family marriages, moral vocabulary, and weekly rhythm are all constituted inside the community, so expressing doubt threatens the entire social world rather than a single opinion.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, doubting_lay_members, payer,
    powerless, biographical, identity_locked, regional).

% Children and teenagers in schools and homeschool programs using creationist curricula. They receive community belonging, moral formation, and a coherent worldview, while absorbing a science education that leaves them unprepared for university science and vulnerable to crisis on first direct contact with the evidence. They cannot exit: parental authority, tuition dependence, and legal minority bind them to the arrangement.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, creationist_schooled_students, payer,
    powerless, immediate, trapped, regional).
narrative_ontology:stakeholder_secondary_role(genesis_creation_narrative__literal_young_earth, creationist_schooled_students, beneficiary).

% Biblical scholars and scientists arguing for ancient Near Eastern genre readings or evolutionary creation. They publish, run their own organizations, and gather at their own conferences, but are barred from the pulpits and classrooms of inerrantist institutions by the same requirement that defines this arrangement; their exclusion is what the enforcement machinery exists to maintain.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, nonliteralist_scholars, excluded,
    organized, biographical, mobile, continental).

% Producers of the radiometric, genomic, cosmological, and geological evidence that the literal reading must reject or reinterpret. It does not participate in the communities governed by the requirement; its verdicts function as the external standard that the governing institutions must either refute publicly or insulate their members from encountering.
narrative_ontology:constraint_stakeholder(genesis_creation_narrative__literal_young_earth, mainstream_scientific_community, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(genesis_creation_narrative__literal_young_earth, creation_apologetics_ministries).
narrative_ontology:fixing_cost_class(genesis_creation_narrative__literal_young_earth, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Fixes one authoritative interpretation of the community's founding text, binding theology, science education, and group identity into a single epistemic framework. It solves a real collective-action problem: without a fixed interpretive anchor, each member decides independently which passages are negotiable, and the community cannot maintain a shared curriculum, a shared confession, or a credible ordination standard. The literal reading supplies the anchor and a bright boundary line.
% TRANSFER_FUNCTION: Moves epistemic deference from individual judgment and from outside scientific institutions to the community's interpretive offices; moves tuition, donations, and book and media revenue toward compliant institutions and ministries; and moves the cost of scientific literacy onto members whose training or honesty will not permit assent, paid in concealed doubt, abandoned vocations, or departure.
% ABSENT_VOICES: Non-literalist scholars and scientifically trained members who were removed, resigned, or never admitted would object that the reading misstates both the text's genre and the evidence; former members who exited would describe the cost of staying. They sit outside the institutions - in independent organizations, secular academies, or no religious community at all - and the enforcement requirement is precisely what keeps them out of the room where the reading is reaffirmed.
% DISAPPEARANCE_RATIONALE: If the requirement vanished overnight, inerrantist institutions would rearrange around a successor reading within a generation - most likely theistic-evolutionary or genre-sensitive readings already circulating - creation ministries would lose their market and consolidate or dissolve, science curricula in affiliated schools would converge on mainstream content, and the doctrinal architecture built on a historical Adam and a recent fall would need formal revision. The communities would survive; their current shape would not.
% FOUNDING_PROBLEM: Preserve the authority of Scripture and the historical fall-redemption narrative against nineteenth-century geology, evolutionary biology, and source criticism: if Genesis 1-2 is not literal history, the argument ran, the historical Adam falls, original sin loses its mechanism, and the atonement loses its foundation.
% FOUNDING_PROBLEM_CORROBORATION: Historians of American religion and of the fundamentalist-modernist controversy, working outside the benefiting parties, corroborate the founding problem's reality in its original context: the reaction to Lyellian deep time and Darwin was documented, organized, and sincerely motivated. Whether the problem remains live today is attested only from inside the movement; historians and philosophers of science outside it widely judge that the threatened collapse of Christianity does not follow from non-literal readings, citing long Augustinian, Catholic, and mainline Protestant traditions that hold non-literal Genesis readings without doctrinal collapse. No corroborating source outside the benefiting parties attests that the founding problem is still live.
narrative_ontology:disappearance_verdict(genesis_creation_narrative__literal_young_earth, world_rearranges).
narrative_ontology:founding_problem_status(genesis_creation_narrative__literal_young_earth, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(genesis_creation_narrative__literal_young_earth, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(genesis_creation_narrative__literal_young_earth, 'none', 1).
narrative_ontology:epsilon_provenance(genesis_creation_narrative__literal_young_earth, 0.7, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(genesis_creation_narrative__literal_young_earth_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(genesis_creation_narrative__literal_young_earth, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(genesis_creation_narrative__literal_young_earth_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Interval units are years: t=0 approximates 1961 (publication of The Genesis Flood, the movement's founding synthesis) through t=60 approximating 2021. Extractiveness 0.70: the transfer is real and concentrated (careers, education, suppressed doubt) but a large share of members experience net benefit, keeping epsilon below snare levels. Suppression 0.80, unscaled by design: employment covenants, doctrinal discipline, curricular control, and social sanction of doubt - persistence depends on actively suppressing sibling readings, not on voluntary preference alone. Theater 0.45: apologetics output performs scientific authority for the faithful more than it produces testable research, while the boundary-maintenance work remains functionally real. Accessibility_collapse 0.60: inside the framework's logic alternatives appear foreclosed (non-literal reading leads to inerrancy collapsing leads to unbelief), yet continuous inter-generational migration to sibling readings shows the exits remain walkable. Resistance 0.55: sustained insider reform organizations, public departures, and generational attrition. All three temporal series share one grid (t=0,10,20,30,40,50,60); the rising trajectories model the enforcement ratchet - post-Scopes institutional hardening, founding of dedicated research institutes, covenant proliferation - and the T17 accumulation trigger may fire on the extractiveness series; that hypothesis is welcome, not tuned away. Suppression_requirement is tracked because enforcement-capacity intensification is a central dynamic of this interval, not a static backdrop.
 *
 * PERSPECTIVAL GAP:
 *   From the leadership and ministry seats the arrangement computes as rope: they built it, staff it, and receive its coordination benefits, and enforcement feels like fidelity. From the faculty, doubter, and student seats the same structure computes as snare-flavored: assent is compelled, alternatives are suppressed, exit is costly. The engine derives these divergent classifications from the power, exit, and role data; the divergence itself - one structure, opposite experiences - is the finding, not an inconsistency to be reconciled.
 *
 * DIRECTIONALITY LOGIC:
 *   Leadership sits near the beneficiary end: it writes and enforces the rules and bears little of their cost, with constrained exit raising its d slightly above a pure beneficiary. Ministries sit nearest the beneficiary end: arbitrage-grade exit and pure collection. Faculty and doubting laity sit near the target end - trapped and identity_locked respectively - bearing the transfer with no offsetting collection; identity lock binds the laity through relational and institutional fusion (the congregation constitutes their entire social world), and vocational-professional fusion binds the faculty. Students are pulled toward the target end by trapped exit despite incidental benefits. Excluded scholars sit outside the enforcement perimeter, so the canonical fallback governs them; no directionality overrides are needed because the beneficiary/victim declarations plus exit options already produce the correct ordering. National-to-global institutional scope modestly amplifies effective extraction through verification difficulty.
 *
 * MANDATROPHY ANALYSIS:
 *   Authoring this as tangled_rope rather than snare prevents mislabeling a sincere, functional identity-coordination arrangement as pure extraction; authoring it as tangled_rope rather than rope keeps the asymmetric costs visible. The R5 interview locates the mandatrophy question precisely: the founding problem (defending Scripture against Darwinism) is experienced as live inside the movement and judged misconceived or resolved outside it, hence status contested. Because disappearance_verdict is world_rearranges while status is contested rather than dead, the zombie/capture mismatch flag should not fire: the arrangement persists because its constitutive problem is disputed, not because its function is gone. If future data showed the founding problem dead even inside the movement - widespread quiet adoption of non-literal readings with no institutional consequence - the mismatch consumer should revisit, since at that point the enforcement machinery would be maintaining performance without a problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_location_of_dispute,
    'Is the disagreement between this reading and its siblings located in the genre of Genesis 1-2 (an empirical-textual question) or in the prior doctrinal commitment to inerrancy-as-historical-chronicle (a conceptual commitment)?',
    'Compare exegesis among scholars who share inerrancy commitments but differ on genre (analogical-days and literary-framework advocates within inerrantism): if inerrantists themselves divide on genre, the dispute is located upstream in the definition of inerrancy, not in the text.',
    'If the dispute is located in the inerrancy definition, this constraint''s persistence depends on doctrinal enforcement rather than textual or empirical considerations, and sibling readings are not empirically refutable from within the framework.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_location_of_dispute, conceptual, 'Committer-frame omega: this story is one reading of the genesis_creation_narrative kernel; whether the reading contest is textual or doctrinal determines what kind of evidence could move it.').

omega_variable(
    persistence_driver_theological_vs_economic,
    'Is the constraint''s persistence driven primarily by theological necessity (the fall-redemption narrative requiring a historical Adam and recent creation) or by institutional economics (ministry revenue, donor retention, employment gatekeeping)?',
    'Comparative institutional analysis: examine denominations with matching theology but no creation-ministry revenue stream, and track whether enforcement intensity tracks revenue dependence across institutions.',
    'If economic, the constraint is more extractive than its coordination framing suggests and reform pressure should target funding structures; if theological, the extraction is a side effect of sincere commitment and enforcement would persist even at financial loss.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(persistence_driver_theological_vs_economic, empirical, 'Whether enforcement intensity follows doctrine or revenue.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (employment covenants, doctrinal discipline, curricular control) or internalized (doubt experienced as sin, habitual self-censorship, fear of divine displeasure)?',
    'Post-exit suppression trajectory: survey members who left inerrantist institutions; if scrupulosity, self-censorship habits, and doubt-as-apostasy framing persist after removal from enforcement structures, a substantial share of the suppression is internalized.',
    'If largely internalized, effective suppression exceeds the structural measure and travels with the member after exit; leaving the institution does not release the constraint''s hold, and exit-based remedies understate the burden.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural versus internalized suppression mechanism split.').

omega_variable(
    identity_content_separability,
    'Does the community''s identity-coordination function require the scientific-claims content (twenty-four-hour days, recent creation), or is the scientific content separable from the boundary-maintenance function?',
    'Compare boundary durability across otherwise similar communities that coordinate identity through non-scientific markers (liturgical practice, creedal minimalism): if identity coordination survives without the young-earth content, the scientific layer is separable and functions as an enforcement surface rather than a coordination core.',
    'If separable, the constraint''s extraction rides on a coordination function it does not itself provide, sharpening the tangled-rope asymmetry and predicting migration paths toward the sibling readings; if inseparable, part of the measured extraction is the price of the coordination itself.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_content_separability, conceptual, 'Whether the identity-coordination framing is genuine or cover for the enforcement content.').

omega_variable(
    dominion_exploitation_causal_link,
    'Does the literalist reading''s dominion interpretation causally license exploitative environmental attitudes among its holders, or is the observed correlation carried by political identity that would persist under the sibling readings?',
    'Within-tradition comparison of environmental attitudes between literalists and theistic evolutionists matched on political identity, plus natural experiments where congregations or institutions shift readings over time.',
    'If causal, this constraint propagates costs beyond its membership into ecological commons and network contamination analysis should weight its edges accordingly; if correlational, that structural delta belongs to a different constraint and should not be charged here.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(dominion_exploitation_causal_link, empirical, 'Whether the dominion-as-exploitation-license delta is caused by this reading or merely correlated with it.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(genesis_creation_narrative__literal_young_earth, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t0, genesis_creation_narrative__literal_young_earth, theater_ratio, 0, 0.15).
narrative_ontology:measurement_basis(gene_tr_t0, observed).
narrative_ontology:measurement(gene_tr_t10, genesis_creation_narrative__literal_young_earth, theater_ratio, 10, 0.18).
narrative_ontology:measurement_basis(gene_tr_t10, observed).
narrative_ontology:measurement(gene_tr_t20, genesis_creation_narrative__literal_young_earth, theater_ratio, 20, 0.22).
narrative_ontology:measurement_basis(gene_tr_t20, observed).
narrative_ontology:measurement(gene_tr_t30, genesis_creation_narrative__literal_young_earth, theater_ratio, 30, 0.27).
narrative_ontology:measurement_basis(gene_tr_t30, observed).
narrative_ontology:measurement(gene_tr_t40, genesis_creation_narrative__literal_young_earth, theater_ratio, 40, 0.33).
narrative_ontology:measurement_basis(gene_tr_t40, observed).
narrative_ontology:measurement(gene_tr_t50, genesis_creation_narrative__literal_young_earth, theater_ratio, 50, 0.39).
narrative_ontology:measurement_basis(gene_tr_t50, observed).
narrative_ontology:measurement(gene_tr_t60, genesis_creation_narrative__literal_young_earth, theater_ratio, 60, 0.45).
narrative_ontology:measurement_basis(gene_tr_t60, observed).

% Extraction over time
narrative_ontology:measurement(gene_be_t0, genesis_creation_narrative__literal_young_earth, base_extractiveness, 0, 0.45).
narrative_ontology:measurement_basis(gene_be_t0, observed).
narrative_ontology:measurement(gene_be_t10, genesis_creation_narrative__literal_young_earth, base_extractiveness, 10, 0.5).
narrative_ontology:measurement_basis(gene_be_t10, observed).
narrative_ontology:measurement(gene_be_t20, genesis_creation_narrative__literal_young_earth, base_extractiveness, 20, 0.56).
narrative_ontology:measurement_basis(gene_be_t20, observed).
narrative_ontology:measurement(gene_be_t30, genesis_creation_narrative__literal_young_earth, base_extractiveness, 30, 0.61).
narrative_ontology:measurement_basis(gene_be_t30, observed).
narrative_ontology:measurement(gene_be_t40, genesis_creation_narrative__literal_young_earth, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(gene_be_t40, observed).
narrative_ontology:measurement(gene_be_t50, genesis_creation_narrative__literal_young_earth, base_extractiveness, 50, 0.68).
narrative_ontology:measurement_basis(gene_be_t50, observed).
narrative_ontology:measurement(gene_be_t60, genesis_creation_narrative__literal_young_earth, base_extractiveness, 60, 0.7).
narrative_ontology:measurement_basis(gene_be_t60, observed).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t0, genesis_creation_narrative__literal_young_earth, suppression_requirement, 0, 0.5).
narrative_ontology:measurement_basis(gene_su_t0, observed).
narrative_ontology:measurement(gene_su_t10, genesis_creation_narrative__literal_young_earth, suppression_requirement, 10, 0.55).
narrative_ontology:measurement_basis(gene_su_t10, observed).
narrative_ontology:measurement(gene_su_t20, genesis_creation_narrative__literal_young_earth, suppression_requirement, 20, 0.6).
narrative_ontology:measurement_basis(gene_su_t20, observed).
narrative_ontology:measurement(gene_su_t30, genesis_creation_narrative__literal_young_earth, suppression_requirement, 30, 0.66).
narrative_ontology:measurement_basis(gene_su_t30, observed).
narrative_ontology:measurement(gene_su_t40, genesis_creation_narrative__literal_young_earth, suppression_requirement, 40, 0.72).
narrative_ontology:measurement_basis(gene_su_t40, observed).
narrative_ontology:measurement(gene_su_t50, genesis_creation_narrative__literal_young_earth, suppression_requirement, 50, 0.77).
narrative_ontology:measurement_basis(gene_su_t50, observed).
narrative_ontology:measurement(gene_su_t60, genesis_creation_narrative__literal_young_earth, suppression_requirement, 60, 0.8).
narrative_ontology:measurement_basis(gene_su_t60, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(genesis_creation_narrative__literal_young_earth, identity_coordination).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative__theistic_evolutionary).
narrative_ontology:affects_constraint(genesis_creation_narrative__literal_young_earth, genesis_creation_narrative__allegorical_ancient_near_east).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'what Genesis 1-2 teaches' conflates three structurally distinct constraints - the enforced literal-historical chronicle (this file, epsilon approximately 0.70, actively enforced, tangled_rope), the theistic-evolutionary framework reading (different enforcement intensity and victim set), and the ancient-Near-Eastern allegorical reading (minimal enforcement, negligible extraction). Each carries its own epsilon, beneficiaries, and stakeholders. The allegorical reading is textually upstream (its genre claims are cited by both others); this reading influences the theistic-evolutionary sibling's operating environment by controlling institutional access and defining the terms of the contest. Linkage here enables contamination-propagation analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
