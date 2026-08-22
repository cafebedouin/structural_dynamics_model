% ============================================================================
% CONSTRAINT STORY: legal_personhood_boundary__functional_capacity_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legal_personhood_boundary__functional_capacity_reading, []).

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
    constraint_indexing:directionality_override/3,
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
 *   constraint_id: legal_personhood_boundary__functional_capacity_reading
 *   human_readable: Functional Capacity Personhood Boundary
 *   domain: legal/philosophical
 *
 * SUMMARY:
 *   This constraint instantiates ONE reading of a contested kernel: the legal
 *   personhood boundary. The functional capacity reading grounds personhood
 *   in demonstrable cognitive capacities (sentience, rationality,
 *   self-awareness) regardless of species membership, contrasting sharply
 *   with the restrictive anthropocentric reading (personhood limited to born
 *   humans) and the developmental potentiality reading (personhood begins at
 *   conception for humans). This reading's adoption would structurally
 *   displace human exceptionalism as a default legal principle, potentially
 *   expanding the victim set to include sentient non-humans currently treated
 *   as property, while closing off legal justifications for species-based
 *   exploitation. The constraint is CLAIMED as tangled_rope (genuine
 *   coordination problem of capacity-based rights attribution, plus
 *   asymmetric extraction from industries currently dependent on property
 *   status) and the metrics reflect the active enforcement required to resist
 *   institutional pressure from industries and traditionalist courts.
 *
 * KEY AGENTS:
 *   - Sentient non-human animals: structurally powerless victims whose sentience would confer standing under this reading but currently bear extraction costs without legal remedy.
 *   - Industrial animal agriculture: institutional payer and agenda-setter actively defending human exceptionalist doctrine to maintain property status of livestock.
 *   - Rights expansion advocates: organized beneficiaries who gain influence and legal precedent through each jurisdiction adopting capacity-based personhood.
 *   - Legal traditionalist courts: institutional agenda-setters enforcing the old boundary by denying standing to non-human plaintiffs.
 *   - European progressive courts: competing agenda-setters partially adopting the reading and creating spillover institutional pressure.
 *   - Human cognitive disability communities: excluded from the kernel dispute but structurally implicated if capacity thresholds become gatekeeping criteria.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, 0.68).
domain_priors:suppression_score(legal_personhood_boundary__functional_capacity_reading, 0.71).
domain_priors:theater_ratio(legal_personhood_boundary__functional_capacity_reading, 0.42).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0.42).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(legal_personhood_boundary__functional_capacity_reading, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legal_personhood_boundary__functional_capacity_reading, tangled_rope).
narrative_ontology:human_readable(legal_personhood_boundary__functional_capacity_reading, "Functional Capacity Personhood Boundary").
narrative_ontology:topic_domain(legal_personhood_boundary__functional_capacity_reading, "legal/philosophical").

domain_priors:requires_active_enforcement(legal_personhood_boundary__functional_capacity_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legal_personhood_boundary__functional_capacity_reading, '8f7b2e98-4f44-42c7-a867-ce60c69554f1').
narrative_ontology:cs_kernel_codification('8f7b2e98-4f44-42c7-a867-ce60c69554f1', formalized).
narrative_ontology:cs_authority_grounding('8f7b2e98-4f44-42c7-a867-ce60c69554f1', extraction).
narrative_ontology:cs_interpretation_layer_present('8f7b2e98-4f44-42c7-a867-ce60c69554f1').
narrative_ontology:cs_reading_relation('8f7b2e98-4f44-42c7-a867-ce60c69554f1', legal_personhood_boundary__restrictive_anthropocentric_reading, coexists_with).
narrative_ontology:cs_reading_relation('8f7b2e98-4f44-42c7-a867-ce60c69554f1', legal_personhood_boundary__developmental_potentiality_reading, coexists_with).
narrative_ontology:cs_axiom('8f7b2e98-4f44-42c7-a867-ce60c69554f1', foundational, sentience_establishes_legal_personhood).
narrative_ontology:cs_axiom_status(sentience_establishes_legal_personhood, holdable).
narrative_ontology:cs_axiom_grounding('8f7b2e98-4f44-42c7-a867-ce60c69554f1', sentience_establishes_legal_personhood, deontological).
narrative_ontology:cs_axiom('8f7b2e98-4f44-42c7-a867-ce60c69554f1', foundational, species_neutrality_in_legal_standing).
narrative_ontology:cs_axiom_status(species_neutrality_in_legal_standing, holdable).
narrative_ontology:cs_axiom_grounding('8f7b2e98-4f44-42c7-a867-ce60c69554f1', species_neutrality_in_legal_standing, deontological).
narrative_ontology:cs_reference_frame('8f7b2e98-4f44-42c7-a867-ce60c69554f1', human_exceptionalist_personhood).
narrative_ontology:cs_drift_state('8f7b2e98-4f44-42c7-a867-ce60c69554f1', contemporary_neuroscience_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('8f7b2e98-4f44-42c7-a867-ce60c69554f1', '').
narrative_ontology:cs_kernel_id(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, sentient_nonhuman_animals).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, future_artificial_intelligences).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, rights_expansion_advocates).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, industrial_animal_agriculture).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, wildlife_exploitation_industries).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, human_exceptionalist_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(legal_personhood_boundary__functional_capacity_reading, philosophical_naturalists).
narrative_ontology:constraint_victim(legal_personhood_boundary__functional_capacity_reading, sentient_nonhuman_animals).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, sentience_as_moral_and_legal_criterion).
narrative_ontology:constraint_vindicates(legal_personhood_boundary__functional_capacity_reading, species_neutrality_in_rights_attribution).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain potential legal standing and rights protection under this reading when their demonstrable sentience is recognized. Currently bear costs of exploitation without remedy or voice in the legal system. Exit is impossible — they cannot organize legal defense or exit jurisdictions. Their cognitive capacities (self-awareness in great apes and cetaceans, pain-responsive behavior in octopi and crustaceans) would establish standing, but legal frameworks do not yet read those capacities as triggering personhood.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, sentient_nonhuman_animals, beneficiary,
    powerless, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__functional_capacity_reading, sentient_nonhuman_animals, payer).

% As-yet-nonexistent but potentially created entities whose functional capacity for rational agency, self-awareness, and preference-formation could establish personhood and rights if this reading becomes institutionalized. Their interest is prospective — the constraint shapes the legal category they may enter.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, future_artificial_intelligences, beneficiary,
    analytical, immediate, analytical, global).

% Currently operates under legal frameworks treating animals as property, enabling high-volume exploitation at low cost to producers. Under functional-capacity reading, confinement practices, slaughter methods, and breeding practices that would constitute legal harm become actionable if the animals' sentience is established. They actively resist the reading through lobbying, narrative defense of human exceptionalism, and legal challenge to animal-standing precedents.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, industrial_animal_agriculture, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(legal_personhood_boundary__functional_capacity_reading, industrial_animal_agriculture, agenda_setter).

% Includes hunting, fur, exotic pet trade, and extractive forestry sectors that depend on treating wild animals and ecosystems as legal objects without standing. Functional capacity reading creates exposure where cognitive capacity of sentient wild populations could ground legal remedies against habitat destruction and unsustainable harvest.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, wildlife_exploitation_industries, payer,
    institutional, generational, constrained, global).

% Animal rights organizations, environmental NGOs, and philosophical advocates who benefit from legal doctrines that recognize non-human sentience as sufficient for rights. Their influence expands with each judicial precedent, legislative reform, or institutional adoption of the reading. They have substantial communication and organizational capacity but limited enforcement power over institutions that resist the shift.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, rights_expansion_advocates, beneficiary,
    organized, generational, mobile, global).

% Legal systems, educational institutions, and cultural authorities that have grounded their legitimacy on the claim that humans hold a unique and categorical status in nature. Functional capacity reading displaces that uniqueness by making sentience and rationality — not species membership — the legal criterion. They resist through doctrinal retrenchment, appeals to tradition, and arguments about the instability of capacity-based criteria.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_exceptionalist_institutions, payer,
    institutional, generational, constrained, global).

% Not at the negotiating table in the kernel dispute, but structurally implicated: functional capacity reading creates potential legal vulnerability for humans whose demonstrable cognitive capacities fall below average thresholds if capacity is made the exclusive criterion. This reading's advocates argue for capacity thresholds low enough to include all humans; excluded communities fear the doctrine could be weaponized against them if the threshold drifts upward.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, human_cognitive_disability_communities, excluded,
    powerless, biographical, identity_locked, national).

% Philosophers and empirical scientists who see functional capacity as the only defensible basis for rights attribution across species boundaries. They benefit from institutional validation of the reading's epistemological grounding. Their work supplies the empirical foundation (ethology, consciousness studies, neuroscience) that would make capacity assessment tractable.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, philosophical_naturalists, beneficiary,
    analytical, civilizational, analytical, universal).

% Judicial and legislative authorities in jurisdictions that have not yet adopted the functional capacity reading. They set the agenda by deciding which cases invoke standing for non-human plaintiffs, which legal doctrines apply, and whether existing statutes permit expansion of personhood categories. Their resistance is the enforcement mechanism that keeps the reading from becoming default law.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, legal_traditionalist_courts, agenda_setter,
    institutional, generational, constrained, national).

% Some European jurisdictions (Switzerland, parts of EU) have begun recognizing animal sentience and animal standing in narrower contexts. These courts are moving enforcement machinery toward the functional capacity reading without fully endorsing it as universal personhood doctrine. Their partial adoption creates a spillover threat to conservative jurisdictions and provides a beachhead for further institutional expansion.
narrative_ontology:constraint_stakeholder(legal_personhood_boundary__functional_capacity_reading, european_progressive_courts, agenda_setter,
    institutional, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(legal_personhood_boundary__functional_capacity_reading, industrial_animal_agriculture).
narrative_ontology:fixing_cost_class(legal_personhood_boundary__functional_capacity_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a unified legal criterion (demonstrable cognitive capacity) for rights attribution that could in principle apply across species and artificial substrates, enabling coherent adjudication of conflicts between sentient beings rather than resolving all disputes by defaulting to human interests.
% TRANSFER_FUNCTION: Transfers legal standing and remedy access from an exclusive human set to a capacity-based set that could include animals and future AIs. Moves the power to define personhood from species membership to empirical demonstration of sentience/rationality. Potentially redistributes resources away from industries dependent on treating sentient beings as property.
% ABSENT_VOICES: Sentient non-human animals cannot speak in legal proceedings; they are represented by advocates whose interests may diverge. Human communities with cognitive disabilities are structurally in the room but marginalized in the kernel dispute, though their interests are directly affected by where capacity thresholds are drawn. Future AIs exist only as counterfactual possibility.
% DISAPPEARANCE_RATIONALE: If functional capacity reading disappeared as a legal doctrine, property regimes over animals and ecosystems would crystallize indefinitely, legal standing for non-human plaintiffs would revert to narrow exceptions, and the institutional barriers to recognizing artificial sentience would persist as default law. The world would rearrange back to species-based legal hierarchy, with cascading effects on animal protection, environmental law, and the legal status of future AI.
% FOUNDING_PROBLEM: Existing personhood doctrine (human exceptionalism, species-based membership) produces systemic inability to address legal harms to demonstrably sentient beings, and creates logical incoherence when confronted with edge cases (marginal humans, great apes, future AI) where species membership and cognitive capacity diverge.
% FOUNDING_PROBLEM_CORROBORATION: Cognitive scientists and ethologists outside benefiting advocacy movements attest that the founding problem — mismatch between law's species criterion and empirical sentience/capacity — is real and growing with neuroscience advances. Animal agriculture industry and legal traditionalists dispute both the problem and the proposed solution. Progressive courts (EU Animal Welfare directives) and some philosophers (Peter Singer, Martha Nussbaum) from outside advocacy-NGO seats corroborate that capacity-based approaches are more coherent than species membership as a legal criterion.
narrative_ontology:disappearance_verdict(legal_personhood_boundary__functional_capacity_reading, world_rearranges).
narrative_ontology:founding_problem_status(legal_personhood_boundary__functional_capacity_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legal_personhood_boundary__functional_capacity_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(legal_personhood_boundary__functional_capacity_reading, 'none', 1).
narrative_ontology:epsilon_provenance(legal_personhood_boundary__functional_capacity_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legal_personhood_boundary__functional_capacity_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legal_personhood_boundary__functional_capacity_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legal_personhood_boundary__functional_capacity_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the reading's legal adoption would redistribute power and resources away from industries currently treating sentient beings as property. At t=0 (before institutional adoption) extractiveness is moderate (0.42) — the reading exists as doctrine in some jurisdictions but lacks enforcement machinery. By t=40, extractiveness rises to 0.68 as progressive courts expand standing and resource reallocation accelerates. Suppression is high (0.71 at t=40) because traditionalist courts and industry actors actively resist the reading through doctrinal retrenchment, lobbying, and appeals to human exceptionalism. Theater ratio rises modestly (0.28 to 0.42) as institutions perform capacity assessment (ethological testimony, neuroscience evidence) to defend their gate-keeping while the functional assessment is genuine. The measurements capture the constraint's trajectory from marginal doctrine (some European precedent, philosophy seminars) to institutionalized legal pressure (standing in progressive courts, resource reallocation from animal agriculture, AI ethics frameworks).
 *
 * PERSPECTIVAL GAP:
 *   Sentient animals and rights advocates compute this as genuine coordination (establishing a coherent, non-arbitrary legal criterion) that benefits them and the animals they represent. Industrial agriculture and traditionalist institutions compute it as pure extraction: a doctrine that threatens their property claims and requires them to absorb costs they did not incur. The gap is structural: the beneficiaries gain access to legal remedies and personhood status, while the payers lose the uncontested right to treat sentient beings as property. A capacity-based assessment could theoretically benefit even traditionalist institutions if they reorganize around it, but the short-term cost of restructuring and the long-term loss of absolute control make resistance rational from that seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Sentient animals: trapped, powerless, identity_locked (they cannot exit their species or escape jurisdictions), so d approaches 1.0 (full targets until the reading spreads). Rights advocates: organized, mobile, beneficiaries of doctrine spread, so d approaches 0.0 (beneficiaries). Industry and traditionalist courts: institutional, constrained exit (cannot simply ignore the doctrine), but currently hold power, so d is complex — the courts are powerful and set the agenda, but the reading structurally targets their capacity to exclude. Directionality override: no standard override needed here because the power atoms (institutional, organized) already capture the structural relationships. The asymmetry comes from beneficiary/victim declarations, not from hidden power dynamics.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is live and contested, not dead. The functional capacity reading is not a zombie doctrine — it has active defenders in progressive courts, animal-rights philosophy, and neuroscience communities. The constraint prevents mandatrophy mislabeling by declaring both the coordination function (establishing a coherent capacity-based criterion) and the extraction function (displacing human exceptionalism and threatening property regimes). The reading is genuinely tangled: no institutional actor fully benefits without accepting redistribution, and no actor fully pays without receiving some systemic coherence benefit.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capacity_threshold_incoherence,
    'If personhood follows demonstrable cognitive capacity, what threshold of capacity triggers legal standing? Where does that threshold fall relative to human cognitive disability variation?',
    'Comparative ethological and neuroscientific assessment of capacity in human disability populations vs. animal species; legal case law setting capacity standards for standing; philosophical consensus on threshold-setting criteria.',
    'If thresholds are set inclusively (low enough to include all humans despite disability), the reading avoids weaponization but faces the philosophical challenge of explaining why some non-humans just below the threshold lack standing. If thresholds exclude any human population, the reading collapses into a covert anthropocentric boundary. This ambiguity is the structural vulnerability the reading must resolve or it defaults back to species membership as a proxy for capacity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(capacity_threshold_incoherence, empirical, 'The operationalization problem: what measurable cognitive capacity qualifies for personhood, and does the threshold accidentally exclude humans or include non-sentient entities?').

omega_variable(
    kernel_reading_foreclosure,
    'Does the functional capacity reading logically foreclose the developmental potentiality reading, or do they merely occupy different institutional positions?',
    'Philosophical and jurisprudential analysis: can a single legal framework hold that personhood is both (a) grounded in demonstrable capacity AND (b) begins at conception for humans? Are these commitments contradictory or can they coexist across different agent populations?',
    'If the readings foreclose each other, the kernel permits only one institutional settlement at a time, and the contest is winner-take-all. If they coexist, the kernel contest is an ongoing jurisdictional disagreement, not a logical contradiction, and both readings can persist in different legal systems indefinitely.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_foreclosure, conceptual, 'Whether functional capacity and developmental potentiality are logically incompatible within a single framework or merely competing institutional commitments.').

omega_variable(
    extractive_industries_coalition_power,
    'Can industrial animal agriculture, wildlife exploitation, and human-exceptionalist institutions form a political coalition powerful enough to suppress the functional capacity reading, or does fragmentation among these interests allow progressive courts to advance the reading despite opposition?',
    'Political economy analysis of lobbying capacity, voting patterns, court appointment timelines, and international regulatory coordination. Observation of whether the coalition holds across threatened industries or fractures when some actors calculate they can capture a better position under the new regime.',
    'Strong coalition suppression keeps the reading confined to philosophical discourse and marginal jurisdictions (theater ratio stays high). Coalition fracture allows institutional spread and real resource reallocation. The suppression metric may be misleading if the coalition is temporally unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extractive_industries_coalition_power, empirical, 'Whether industrial and institutional opposition to the functional capacity reading is durable or vulnerable to defection.').

omega_variable(
    reading_committer_identity,
    'Is the FUNCTIONAL_CAPACITY_READING this constraint instantiates the same capacity-based boundary that animal rights philosophers (Singer, Regan) and EU directives declare, or is there a deeper philosophical disagreement about whether capacity grounds moral status vs. legal personhood specifically?',
    'Textual analysis of foundational philosophical sources vs. case law in progressive jurisdictions; examination of whether the reading''s legal application remains faithful to its philosophical grounding or drifts into a narrower gate.',
    'If the reading is faithful to its philosophical grounding, philosophers and courts are in genuine alignment and the constraint''s classification is stable. If institutional adoption narrows the reading (e.g., courts recognize capacity in great apes but deny it to dolphins or future AI), the reading bifurcates and this story should be decomposed into institutional and philosophical variants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_committer_identity, conceptual, 'Whether the legal instantiation of capacity-based personhood stays true to its philosophical origins or diverges during institutional adoption.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legal_personhood_boundary__functional_capacity_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lega_tr_t0, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(lega_tr_t0, projected).
narrative_ontology:measurement(lega_tr_t8, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 8, 0.32).
narrative_ontology:measurement_basis(lega_tr_t8, observed).
narrative_ontology:measurement(lega_tr_t16, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 16, 0.37).
narrative_ontology:measurement_basis(lega_tr_t16, observed).
narrative_ontology:measurement(lega_tr_t24, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 24, 0.4).
narrative_ontology:measurement_basis(lega_tr_t24, observed).
narrative_ontology:measurement(lega_tr_t32, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 32, 0.41).
narrative_ontology:measurement_basis(lega_tr_t32, observed).
narrative_ontology:measurement(lega_tr_t40, legal_personhood_boundary__functional_capacity_reading, theater_ratio, 40, 0.42).
narrative_ontology:measurement_basis(lega_tr_t40, observed).

% Extraction over time
narrative_ontology:measurement(lega_be_t0, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(lega_be_t0, projected).
narrative_ontology:measurement(lega_be_t8, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement_basis(lega_be_t8, observed).
narrative_ontology:measurement(lega_be_t16, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 16, 0.56).
narrative_ontology:measurement_basis(lega_be_t16, observed).
narrative_ontology:measurement(lega_be_t24, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 24, 0.63).
narrative_ontology:measurement_basis(lega_be_t24, observed).
narrative_ontology:measurement(lega_be_t32, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 32, 0.67).
narrative_ontology:measurement_basis(lega_be_t32, observed).
narrative_ontology:measurement(lega_be_t40, legal_personhood_boundary__functional_capacity_reading, base_extractiveness, 40, 0.68).
narrative_ontology:measurement_basis(lega_be_t40, observed).

% Suppression requirement over time
narrative_ontology:measurement(lega_su_t0, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(lega_su_t0, projected).
narrative_ontology:measurement(lega_su_t8, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 8, 0.54).
narrative_ontology:measurement_basis(lega_su_t8, observed).
narrative_ontology:measurement(lega_su_t16, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 16, 0.61).
narrative_ontology:measurement_basis(lega_su_t16, observed).
narrative_ontology:measurement(lega_su_t24, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 24, 0.66).
narrative_ontology:measurement_basis(lega_su_t24, observed).
narrative_ontology:measurement(lega_su_t32, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 32, 0.69).
narrative_ontology:measurement_basis(lega_su_t32, observed).
narrative_ontology:measurement(lega_su_t40, legal_personhood_boundary__functional_capacity_reading, suppression_requirement, 40, 0.71).
narrative_ontology:measurement_basis(lega_su_t40, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legal_personhood_boundary__functional_capacity_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(legal_personhood_boundary__functional_capacity_reading, 0.12).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__restrictive_anthropocentric_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, legal_personhood_boundary__developmental_potentiality_reading).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, animal_property_status_doctrine).
narrative_ontology:affects_constraint(legal_personhood_boundary__functional_capacity_reading, ai_consciousness_legal_standing).

% DUAL FORMULATION NOTE:
% The legal_personhood_boundary kernel generates three distinct constraint stories: this functional_capacity_reading, plus restrictive_anthropocentric_reading and developmental_potentiality_reading. Each reading carries its own ε (extractiveness relative to the standing arrangement under that reading's lights), its own beneficiary/victim set, and its own enforcement profile. They coexist as live institutional positions held by different jurisdictions and courts. Decomposition is necessary because ε-value diverges: functional capacity reading sees high extraction in the species-membership status quo; developmental potentiality reading sees low extraction (that status quo aligns with its principles); restrictive anthropocentric reading sees moderate extraction (it shifts the extraction target but retains human privilege). One reading cannot hold all three ε-values simultaneously.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, powerless, 0.98).
constraint_indexing:directionality_override(legal_personhood_boundary__functional_capacity_reading, institutional, 0.45).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
