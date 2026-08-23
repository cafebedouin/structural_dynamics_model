% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_commitment__symbolic_transformation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-06
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_commitment__symbolic_transformation, []).

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
 *   constraint_id: temple_sacrifice_commitment__symbolic_transformation
 *   human_readable: Post-Temple Transformation of the Sacrifice Commitment (Symbolic Transformation Reading)
 *   domain: religious law / halakhic tradition / commitment-system theory
 *
 * SUMMARY:
 *   After Rome destroyed the Jerusalem Temple, a binding sacrificial
 *   commandment lost its material preconditions, and the surviving rabbinic
 *   leadership answered with a redefinition: prayer and the study of the
 *   sacrificial laws are the commitment's new instantiation, not stand-ins
 *   for a suspended duty. This story instantiates ONE reading of that
 *   contested kernel — the symbolic_transformation reading — and authors it
 *   as a clean, epsilon-invariant constraint per the family-decomposition
 *   rule: sibling readings (performance_only, study_as_exercise,
 *   hybrid_preparatory) are separate stories with their own epsilon values
 *   and victim sets, linked through the network edges. Structurally, the
 *   reading presents a genuine coordination achievement wrapped around real
 *   asymmetry: interpretive authority concentrates in the rabbinic class, the
 *   hereditary priesthood loses its material sacral economy while retaining
 *   honors, and those who hold material performance non-negotiable are
 *   subordinated as the price of communal unity. The claimed type and the
 *   metrics are independent authored facts: the tangled-rope claim is what
 *   the structure appears to be from the authoring seat; the metric values
 *   describe the arrangement's operation as the historical record supports,
 *   and any divergence between them is signal, not error.
 *
 * KEY AGENTS:
 *   - rabbinic_sages: agenda-setting beneficiary (institutional / identity_locked) — declares and administers the transformation; deference, judicial centrality, and academy patronage flow to this seat
 *   - diaspora_talmudic_academies: institutional beneficiary (institutional / constrained) — inherit the Temple's former centrality as the sites where sacrificial law is studied and adjudicated
 *   - lay_praying_community: dual beneficiary-payer (organized / constrained) — receives a workable covenantal practice in exchange for academy support and submission to rabbinic discipline
 *   - priestly_line: payer (moderate / identity_locked) — hereditary custodians of the sacrificial rite, displaced from its material economy, retained for liturgical honors
 *   - literalist_restorationists: payer (powerless / trapped) — hold material performance non-negotiable; their position is delegitimized inside the framework as disunity rather than fidelity
 *   - karaite_scripturalists: payer (organized / mobile) — rejected the transformation wholesale, accepting schism and exclusion as the cost of their position
 *   - samaritan_sacrifice_community: excluded voice (organized / trapped) — maintained actual sacrifice on Gerizim throughout; never party to the rabbinic conversation their practice refutes
 *   - academic_historians_of_judaism: analytical observer (analytical / analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, 0.62).
domain_priors:suppression_score(temple_sacrifice_commitment__symbolic_transformation, 0.64).
domain_priors:theater_ratio(temple_sacrifice_commitment__symbolic_transformation, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, extractiveness, 0.62).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(temple_sacrifice_commitment__symbolic_transformation, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_commitment__symbolic_transformation, tangled_rope).
narrative_ontology:human_readable(temple_sacrifice_commitment__symbolic_transformation, "Post-Temple Transformation of the Sacrifice Commitment (Symbolic Transformation Reading)").
narrative_ontology:topic_domain(temple_sacrifice_commitment__symbolic_transformation, "religious law / halakhic tradition / commitment-system theory").

domain_priors:requires_active_enforcement(temple_sacrifice_commitment__symbolic_transformation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_commitment__symbolic_transformation, '31efe77e-051d-46de-8fe1-7206146f70c3').
narrative_ontology:cs_kernel_codification('31efe77e-051d-46de-8fe1-7206146f70c3', fixed_text).
narrative_ontology:cs_authority_grounding('31efe77e-051d-46de-8fe1-7206146f70c3', lineage).
narrative_ontology:cs_interpretation_layer_present('31efe77e-051d-46de-8fe1-7206146f70c3').
narrative_ontology:cs_reading_relation('31efe77e-051d-46de-8fe1-7206146f70c3', temple_sacrifice_commitment__performance_only, forecloses).
narrative_ontology:cs_reading_relation('31efe77e-051d-46de-8fe1-7206146f70c3', temple_sacrifice_commitment__hybrid_preparatory, forecloses).
narrative_ontology:cs_reading_relation('31efe77e-051d-46de-8fe1-7206146f70c3', temple_sacrifice_commitment__study_as_exercise, coexists_with).
narrative_ontology:cs_axiom('31efe77e-051d-46de-8fe1-7206146f70c3', foundational, verbal_worship_divinely_equivalent).
narrative_ontology:cs_axiom_status(verbal_worship_divinely_equivalent, holdable).
narrative_ontology:cs_axiom_grounding('31efe77e-051d-46de-8fe1-7206146f70c3', verbal_worship_divinely_equivalent, theological).
narrative_ontology:cs_axiom('31efe77e-051d-46de-8fe1-7206146f70c3', foundational, rabbinic_jurisdiction_over_instantiation_mode).
narrative_ontology:cs_axiom_status(rabbinic_jurisdiction_over_instantiation_mode, holdable).
narrative_ontology:cs_axiom_grounding('31efe77e-051d-46de-8fe1-7206146f70c3', rabbinic_jurisdiction_over_instantiation_mode, conventional).
narrative_ontology:cs_reference_frame('31efe77e-051d-46de-8fe1-7206146f70c3', adaptively_instantiated_covenant_command).
narrative_ontology:cs_drift_state('31efe77e-051d-46de-8fe1-7206146f70c3', contemporary_restoration_pressure_era, gap(repudiation_pressure, minor, true)).
narrative_ontology:cs_created_at('31efe77e-051d-46de-8fe1-7206146f70c3', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, rabbinic_sages).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, diaspora_talmudic_academies).
narrative_ontology:constraint_beneficiary(temple_sacrifice_commitment__symbolic_transformation, lay_praying_community).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, priestly_line).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, literalist_restorationists).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, karaite_scripturalists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(temple_sacrifice_commitment__symbolic_transformation, lay_praying_community).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, oral_torah_interpretive_supremacy).
narrative_ontology:constraint_vindicates(temple_sacrifice_commitment__symbolic_transformation, lips_offering_equivalence_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Declare and administer the transformation: define which prayers and which studies count as fulfilling the sacrificial commandments, train and ordain successors, and adjudicate disputes over the transformed practice. Deference, judicial business, and academy patronage flow to this seat. Their personal authority, livelihood, and life's work are constituted by the interpretive role itself; renouncing the transformation would mean repudiating their own standing and the chain that authorized them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, rabbinic_sages, agenda_setter,
    institutional, generational, identity_locked, continental).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, rabbinic_sages, beneficiary).

% Great study houses in the diaspora centers inherit the centrality the ruined Temple vacated: enrollment, endowed support, and the prestige of housing the definitive study of the sacrificial laws concentrate at these institutions. Leaving that role would mean surrendering the patronage and studentship that sustain them, so their position inside the arrangement is secured by the resources the arrangement channels to them.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, diaspora_talmudic_academies, beneficiary,
    institutional, generational, constrained, continental).

% Ordinary households receive a covenantal life that remains livable without the Temple: fixed prayers framed as offerings, calendar rites, and a coherent answer to the unanswerable commandment. They pay for this with communal dues redirected toward academies and courts, with conformity to standardized liturgy, and with the quiet subordination of private hopes for literal restoration, which communal discipline treats as divisive. Exiting means leaving the community entire.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, lay_praying_community, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(temple_sacrifice_commitment__symbolic_transformation, lay_praying_community, payer).

% Hereditary custodians of the sacrificial rite, descended through families whose status, training, and economy were organized around the altar. The transformation reassigns the rite's fulfillment to prayer and study conducted under rabbinic authority, leaving the priests liturgical honors — the blessing, precedence in reading, vestiges of purity practice — while the interpretive and economic center of gravity passes to the sages. They cannot exit their lineage, and remaining inside the covenant means accepting the reassignment their displacement rides on.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, priestly_line, payer,
    moderate, generational, identity_locked, regional).

% In every generation some hold that the sacrificial commandments demand material performance and that no verbal practice fulfills them. Inside the framework they cannot perform what they believe binding, and they may not abandon the covenant either; their position is treated as a threat to unity, met with argument, discipline, and social pressure rather than accommodation. They bear the arrangement's enforcement while receiving none of the settlement's benefits, and each generation reproduces them anew.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, literalist_restorationists, payer,
    powerless, biographical, trapped, regional).

% Communities that rejected the entire rabbinic claim to bind practice through transmitted interpretation, insisting on scripture alone — which returns them to the sacrificial laws in their material form and to the conclusion that the transformation was an usurpation. They organized their own congregations, calendars, and courts, and paid for the exit in schism: exclusion from the shared institutions, intermarriage barriers, and anathema exchanged with the rabbinic majority. Their mobility is real — they left — but the leaving itself is the cost they bear.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, karaite_scripturalists, payer,
    organized, generational, mobile, regional).

% A sister covenant-community that never accepted the rabbinic framework and never stopped offering actual sacrifice on its sacred mountain, through the destruction and long after. Its continuous material practice is a standing counterexample to any claim that sacrifice became impossible or that verbal worship was its necessary successor — yet it was never party to the rabbinic conversation its existence quietly refutes. It has its own trap: its practice depends on a single site and a small, closed population.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, samaritan_sacrifice_community, excluded,
    organized, generational, trapped, regional).

% Scholars outside the confessional commitments reconstruct how the post-destruction settlement formed: what the destruction made impossible, what the early authorities chose, whom the settlement advantaged, and how dissent was handled. They read the polemical literature from every side, weigh the archaeological and documentary record, and publish analyses that no seat controls — including analyses favorable and unfavorable to each party's self-understanding.
narrative_ontology:constraint_stakeholder(temple_sacrifice_commitment__symbolic_transformation, academic_historians_of_judaism, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(temple_sacrifice_commitment__symbolic_transformation, rabbinic_sages).
narrative_ontology:fixing_cost_class(temple_sacrifice_commitment__symbolic_transformation, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the crisis of the unfulfillable commandment: after the Temple's destruction, a binding sacrificial obligation had no material preconditions, and a dispersed people needed one authoritative answer to 'how is this commandment still fulfilled?' — the transformation supplies a single shared practice (fixed prayer framed as offering, standardized study of the sacrificial laws) that keeps covenantal life coherent across scattered communities instead of fragmenting into rival private answers.
% TRANSFER_FUNCTION: Moves interpretive authority, judicial centrality, and institutional patronage from the hereditary priesthood and the destroyed Temple to the rabbinic class, its academies, and its courts; moves the conformity of dissenters (restorationist conviction subordinated to communal unity) and the material support of households toward the academy-and-court complex.
% ABSENT_VOICES: The Samaritan community, still offering material sacrifice throughout, would deny both the impossibility premise and the authorization claim, and was never admitted to the conversation; restorationist minorities inside the communities objected but lacked a seat, their objections processed as discipline cases; the Sadducean priestly opposition to oral interpretive authority had already been broken before the transformation was articulated, so its heirs were not present to contest the jurisdiction the arrangement assumes.
% DISAPPEARANCE_RATIONALE: If the transformation and its enforcement vanished overnight, the covenant's answer to the unperformable commandment disappears with it: the fixed liturgy loses its framing logic, the academies lose the curriculum that anchors their centrality, the rabbinic class loses the jurisdiction that constitutes its authority, and the communities must either improvise a replacement settlement, fracture along restorationist-versus-adaptation lines, or abandon the obligation entirely — the entire post-Temple religious economy reorganizes around whichever answer wins.
% FOUNDING_PROBLEM: Built to solve: after 70 CE, a divinely commanded sacrificial system became materially impossible while the obligation itself remained in force, confronting a defeated, dispersed community with despair, fragmentation, or covenantal abandonment; the arrangement was constructed so the covenant could remain intact and governable without its altar.
% FOUNDING_PROBLEM_CORROBORATION: Corroboration exists outside the beneficiary set: the physical and textual record of the Temple's destruction (Roman-era accounts and the archaeological record) attests that the material preconditions really failed; the Samaritan community's uninterrupted material sacrifice independently confirms that the crisis was specific to the Jerusalem-centered rite rather than a rabbinic invention; and academic historiography of the period — no seat of which benefits from the arrangement — corroborates both the severity of the founding crisis and the contested character of the settlement. Rabbinic texts attest the problem as well, but they are the benefiting parties' own testimony and are weighted accordingly.
narrative_ontology:disappearance_verdict(temple_sacrifice_commitment__symbolic_transformation, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_commitment__symbolic_transformation, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_commitment__symbolic_transformation, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(temple_sacrifice_commitment__symbolic_transformation, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_commitment__symbolic_transformation, 0.62, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(temple_sacrifice_commitment__symbolic_transformation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(temple_sacrifice_commitment__symbolic_transformation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction climbs from 0.42 to roughly 0.62 across the interval as interpretive authority consolidates — codification, the academy system, and the Geonate each layer institutional advantage onto what began as a crisis response; the late plateau reflects normalization rather than reform. Suppression traces a genuine enforcement-capacity arc, which is why suppression_requirement is tracked: charismatic authority sufficed at the outset, court discipline and the ban machinery hardened through codification, peaked at the Karaite schism, and settled as compliance internalized. Theater grows steadily as liturgical re-enactment of the Temple order acquires memorial character, but stays below function dominance — the practice still solves the living problem it addresses. All three series run on one shared time grid so every metric is authored at every examined time point. Suppression is authored as a raw structural property, unscaled; only extractiveness is context-scaled downstream. Resistance at 0.55 reflects sustained contest — Sadducean remnants, recurring restorationism, and the Karaite rupture — and accessibility_collapse at 0.50 marks the boundary the Karaite exit demonstrated: within the framework alternatives collapse almost completely, but the frame itself could be left at a price.
 *
 * PERSPECTIVAL GAP:
 *   From the rabbinic seat the arrangement is fidelity: the covenant kept whole through authorized adaptation, with the authority structure bearing the maintenance burden its role imposes. From the priestly seat the same structure is displacement ratified as doctrine — their hereditary function reassigned to a class that then wrote the rules of reassignment. From the restorationist seat it is an unauthorized redefinition they are compelled to subsidize with conformity. The engine computes per-seat classifications from the structural data; the authored claim does not adjudicate among these experiences, and the gap between them is the measurement the corpus exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   The rabbinic seats sit near the beneficiary end: they collect interpretive supremacy, judicial centrality, and academy patronage from the arrangement they author. The lay praying community sits near symmetric — it receives a genuinely workable covenantal practice while paying dues, conformity, and the marginalization of its own restorationist impulses. The priestly line sits near the target end, amplified by identity lock: lineage cannot be exited, so displacement costs cannot be escaped by leaving. Literalist restorationists sit nearest the full-target end: trapped between an impossible performance and a forbidden abandonment, they bear the constraint's discipline without receiving its benefits. Karaite scripturalists bear real exclusion costs but their mobile exit blunts effective extraction relative to the trapped. The Samaritan community is outside the derivation entirely — excluded rather than coordinated — and its continued material sacrifice is the standing counterexample the arrangement's enforcement exists to contain rhetorically.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification guards against two opposite mislabels. Reading the arrangement as pure coordination (rope) ignores the asymmetry: authority capture over the command's instantiation mode, uncompensated priestly displacement, and the coerced conformity of dissent — extraction riding the same structure that coordinates. Reading it as pure extraction (snare) ignores what was genuinely solved: a covenant facing an unperformable commandment needed a single authoritative answer or it would have fragmented into rival answers and probable dissolution. The mandate is still live while the Temple stands unbuilt, so no sunset applies and no piton decay is claimed; the rising theater_ratio is monitored as the leading indicator of memorialization displacing function, but the arrangement's activity still tracks its founding problem rather than performing it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    authorization_status_omega,
    'Was the transformation of the sacrifice commitment into prayer-and-study instantiation authorized within the covenant''s own jurisdiction, or is it unauthorized drift in which the rabbinic authority seized power to redefine a divine command?',
    'Comparative canonical analysis: whether the scriptural corpus (the lips-offering passages and parallels) together with the transmitted interpretive chain grounds jurisdiction over instantiation modes, tested against criteria the tradition itself accepts as licensing reinterpretation.',
    'If unauthorized, effective extraction rises sharply for every bound seat, the classification moves toward pure extraction, and the victim set expands to all participants bound without consent; if authorized, extraction is bounded by the coordination function it prices and the tangled-rope reading holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(authorization_status_omega, conceptual, 'Whether the transformation carries jurisdictional authorization or constitutes a power seizure over divine-command content.').

omega_variable(
    kernel_sibling_delta,
    'This story instantiates one reading of the temple_sacrifice_commitment kernel; how would the sibling readings (performance_only, hybrid_preparatory, study_as_exercise) alter epsilon and the victim set?',
    'Author each sibling as a separate constraint story and compare computed classifications; the disagreement is located in the commitment''s present status — occupied now (this reading), suspended and merely maintained (hybrid_preparatory), archived as defunct (performance_only), or performed intellectually (study_as_exercise).',
    'Under performance_only the arrangement preserves a defunct practice with a different epsilon referent and persistence semantics; under hybrid_preparatory the arrangement gains transitional, sunset-facing character; under study_as_exercise the intellectual seat''s burden lightens. Victim sets shift accordingly.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_sibling_delta, conceptual, 'Cross-reading structural delta for the kernel family.').

omega_variable(
    consent_of_bound_generation,
    'Did the generation actually bound by the original material commandment consent to its redefinition, or does the transformation bind retroactively without any consent mechanism?',
    'Historical reconstruction of adoption patterns across the first two post-destruction centuries: voluntary uptake under shared catastrophe versus imposition by the surviving authoritative remnant, using documentary and epigraphic evidence of dissent and acquiescence.',
    'If retroactive binding without consent is decisive, extraction amplifies across all payer seats and the suppression profile reads as manufactured rather than crisis-necessary; if catastrophic circumstances carried genuine assent, the measured extraction reflects priced coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_of_bound_generation, empirical, 'Consent status of the transformation for the originally bound population.').

omega_variable(
    priestly_displacement_terms,
    'Were the priestly line''s losses under the transformation compensated (retained honors, redirected dues, economic niches) or borne uncompensated?',
    'Analysis of post-destruction priestly economic roles: allocation of liturgical honors, treatment of first-fruits and tithe streams, and settlement patterns of priestly families relative to academy centers.',
    'Uncompensated displacement pushes the priestly seat toward the full-target end and widens the victim set''s severity; compensated retention moderates it and narrows the asymmetry the arrangement must defend.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(priestly_displacement_terms, empirical, 'Severity and compensation of priestly displacement under the transformed arrangement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_commitment__symbolic_transformation, 0, 800).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(temp_tr_t0, observed).
narrative_ontology:measurement(temp_tr_t60, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 60, 0.16).
narrative_ontology:measurement_basis(temp_tr_t60, observed).
narrative_ontology:measurement(temp_tr_t130, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 130, 0.21).
narrative_ontology:measurement_basis(temp_tr_t130, observed).
narrative_ontology:measurement(temp_tr_t300, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 300, 0.27).
narrative_ontology:measurement_basis(temp_tr_t300, observed).
narrative_ontology:measurement(temp_tr_t480, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 480, 0.32).
narrative_ontology:measurement_basis(temp_tr_t480, observed).
narrative_ontology:measurement(temp_tr_t650, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 650, 0.35).
narrative_ontology:measurement_basis(temp_tr_t650, observed).
narrative_ontology:measurement(temp_tr_t720, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 720, 0.37).
narrative_ontology:measurement_basis(temp_tr_t720, observed).
narrative_ontology:measurement(temp_tr_t800, temple_sacrifice_commitment__symbolic_transformation, theater_ratio, 800, 0.38).
narrative_ontology:measurement_basis(temp_tr_t800, observed).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(temp_be_t0, observed).
narrative_ontology:measurement(temp_be_t60, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 60, 0.46).
narrative_ontology:measurement_basis(temp_be_t60, observed).
narrative_ontology:measurement(temp_be_t130, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 130, 0.5).
narrative_ontology:measurement_basis(temp_be_t130, observed).
narrative_ontology:measurement(temp_be_t300, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 300, 0.54).
narrative_ontology:measurement_basis(temp_be_t300, observed).
narrative_ontology:measurement(temp_be_t480, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 480, 0.58).
narrative_ontology:measurement_basis(temp_be_t480, observed).
narrative_ontology:measurement(temp_be_t650, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 650, 0.61).
narrative_ontology:measurement_basis(temp_be_t650, observed).
narrative_ontology:measurement(temp_be_t720, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 720, 0.63).
narrative_ontology:measurement_basis(temp_be_t720, observed).
narrative_ontology:measurement(temp_be_t800, temple_sacrifice_commitment__symbolic_transformation, base_extractiveness, 800, 0.62).
narrative_ontology:measurement_basis(temp_be_t800, observed).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 0, 0.4).
narrative_ontology:measurement_basis(temp_su_t0, observed).
narrative_ontology:measurement(temp_su_t60, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 60, 0.49).
narrative_ontology:measurement_basis(temp_su_t60, observed).
narrative_ontology:measurement(temp_su_t130, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 130, 0.55).
narrative_ontology:measurement_basis(temp_su_t130, observed).
narrative_ontology:measurement(temp_su_t300, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 300, 0.58).
narrative_ontology:measurement_basis(temp_su_t300, observed).
narrative_ontology:measurement(temp_su_t480, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 480, 0.6).
narrative_ontology:measurement_basis(temp_su_t480, observed).
narrative_ontology:measurement(temp_su_t650, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 650, 0.57).
narrative_ontology:measurement_basis(temp_su_t650, observed).
narrative_ontology:measurement(temp_su_t720, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 720, 0.66).
narrative_ontology:measurement_basis(temp_su_t720, observed).
narrative_ontology:measurement(temp_su_t800, temple_sacrifice_commitment__symbolic_transformation, suppression_requirement, 800, 0.64).
narrative_ontology:measurement_basis(temp_su_t800, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_commitment__symbolic_transformation, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__study_as_exercise).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__performance_only).
narrative_ontology:affects_constraint(temple_sacrifice_commitment__symbolic_transformation, temple_sacrifice_commitment__hybrid_preparatory).

% DUAL FORMULATION NOTE:
% The colloquial label 'what happened to the sacrifice commandment after the Temple fell' decomposes into four structurally distinct constraint stories under the epsilon-invariance principle: symbolic_transformation (this file — authorized re-instantiation, bounded extraction, tangled-rope shape), performance_only (arrangement preserves a defunct practice pending restoration), study_as_exercise (intellectual performance occupies the command now), and hybrid_preparatory (preparatory maintenance of a suspended commitment, sunset-facing). Each carries a single stable epsilon over its own referent and victim set. The upstream reading (symbolic_transformation) is the historically dominant institutional settlement and creates the operating conditions — liturgy, academy curriculum, enforcement norms — within which the siblings are articulated; edges run from this file to all three.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
