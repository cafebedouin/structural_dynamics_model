% ============================================================================
% CONSTRAINT STORY: dignified_death__autonomy_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dignified_death__autonomy_primary, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: dignified_death__autonomy_primary
 *   human_readable: Autonomy-Primary Dignity Norm in End-of-Life Decision Authority
 *   domain: bioethics/medical_law/political_philosophy
 *
 * SUMMARY:
 *   This constraint captures the autonomy-primary reading of the
 *   dignified_death kernel: the assertion that dignity resides fundamentally
 *   in self-determination, and that a suffering individual possesses final
 *   authority over the timing and method of their death. This reading has
 *   gained legal codification in multiple jurisdictions (Netherlands,
 *   Belgium, Switzerland, Canada, Spain, some US states) over the past 50
 *   years. It instantiates a tangled rope: genuine coordination function
 *   (removes ambiguity about who decides) entangled with asymmetric
 *   extraction (those denied exit pay the cost of prolonged suffering against
 *   their will, and the constraint's persistence depends on suppressing
 *   alternative readings and gatekeeping authority). The reading is one of
 *   three contending approaches to dignity in end-of-life contexts; the
 *   others (sanctity_primary, relational_autonomy) operate as separate
 *   constraints with different victim sets and epsilon values. The
 *   autonomy-primary reading is live and expanding; the foundational problem
 *   (who decides when I die?) remains contested.
 *
 * KEY AGENTS:
 *   - terminally_suffering_autonomous_agent — the beneficiary framed in autonomy-primary reading; experiences the constraint as recognizing their dignity through authority over death
 *   - suffering_individual_denied_exit — the victim; trapped in prolonged suffering when prohibition or gatekeeping denies their request to die
 *   - medical_professionals — agenda-setter with gatekeeping power; assess eligibility and capacity; caught between preservation duty and autonomy norm; constrained exit_options
 *   - state_prohibition_authority — agenda-setter in prohibition jurisdictions; articulates and enforces the rule that neither individuals nor clinicians have authority to intentionally end life
 *   - disability_rights_advocates — excluded voice; object to expansion to non-terminal disability; experience policy as coercive culling masked as choice
 *   - religious_and_sanctity_communities — excluded from autonomy-primary authority structure; live in legal regime they experience as fundamentally illegitimate
 *   - relational_autonomy_theorists — excluded analytical voice; argue for distributed decision authority across patient-family-clinician triad
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dignified_death__autonomy_primary, 0.52).
domain_priors:suppression_score(dignified_death__autonomy_primary, 0.71).
domain_priors:theater_ratio(dignified_death__autonomy_primary, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, extractiveness, 0.52).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, suppression_requirement, 0.71).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(dignified_death__autonomy_primary, resistance, 0.74).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dignified_death__autonomy_primary, tangled_rope).
narrative_ontology:human_readable(dignified_death__autonomy_primary, "Autonomy-Primary Dignity Norm in End-of-Life Decision Authority").
narrative_ontology:topic_domain(dignified_death__autonomy_primary, "bioethics/medical_law/political_philosophy").

domain_priors:requires_active_enforcement(dignified_death__autonomy_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(dignified_death__autonomy_primary, '26c96ebc-3995-4cd5-bf27-f964ade6d46e').
narrative_ontology:cs_kernel_codification('26c96ebc-3995-4cd5-bf27-f964ade6d46e', fixed_text).
narrative_ontology:cs_authority_grounding('26c96ebc-3995-4cd5-bf27-f964ade6d46e', extraction).
narrative_ontology:cs_interpretation_layer_present('26c96ebc-3995-4cd5-bf27-f964ade6d46e').
narrative_ontology:cs_reading_relation('26c96ebc-3995-4cd5-bf27-f964ade6d46e', dignified_death__sanctity_primary, coexists_with).
narrative_ontology:cs_reading_relation('26c96ebc-3995-4cd5-bf27-f964ade6d46e', dignified_death__relational_autonomy, influences).
narrative_ontology:cs_axiom('26c96ebc-3995-4cd5-bf27-f964ade6d46e', foundational, dignity_equals_self_determination).
narrative_ontology:cs_axiom_status(dignity_equals_self_determination, holdable).
narrative_ontology:cs_axiom_grounding('26c96ebc-3995-4cd5-bf27-f964ade6d46e', dignity_equals_self_determination, deontological).
narrative_ontology:cs_axiom('26c96ebc-3995-4cd5-bf27-f964ade6d46e', foundational, individual_death_authority_supremacy).
narrative_ontology:cs_axiom_status(individual_death_authority_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('26c96ebc-3995-4cd5-bf27-f964ade6d46e', individual_death_authority_supremacy, deontological).
narrative_ontology:cs_reference_frame('26c96ebc-3995-4cd5-bf27-f964ade6d46e', autonomous_individual_as_sole_authority).
narrative_ontology:cs_drift_state('26c96ebc-3995-4cd5-bf27-f964ade6d46e', contemporary_medical_aid_in_dying_codification_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('26c96ebc-3995-4cd5-bf27-f964ade6d46e', '').
narrative_ontology:cs_kernel_id(dignified_death__autonomy_primary, dignified_death).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dignified_death__autonomy_primary, terminally_suffering_autonomous_agent).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, suffering_individual_denied_exit).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, medical_professionals).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, disability_rights_advocates).
narrative_ontology:constraint_victim(dignified_death__autonomy_primary, surviving_family_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A person facing terminal illness or unbearable suffering who retains decisional capacity. This reading affirms that their choice about timing and method of death expresses their dignity and self-determination. They receive recognition of their autonomous will as the authoritative voice on their own death. Their exit from the living state is framed as an exercise of autonomous choice, not a medical failure. Dignity is realized through being heard and obeyed.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, terminally_suffering_autonomous_agent, beneficiary,
    moderate, immediate, identity_locked, local).

% A person in terminal suffering or existential distress whose request to end their life is refused or delayed by legal prohibition, medical gatekeeping, or clinician objection. They are denied the exercise of self-determination and forced to continue living against their stated will. The constraint extracts from them: prolonged suffering, loss of autonomous agency, and the experience of their will being overridden by state or medical authority. They pay the cost in lost autonomy and endured pain.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, suffering_individual_denied_exit, payer,
    powerless, immediate, trapped, local).

% Physicians, nurses, and other clinicians who must adjudicate eligibility, assess decisional capacity, and potentially provide or refuse assistance with death. In jurisdictions where the autonomy-primary reading is codified, they hold gatekeeping power: they diagnose terminal illness, assess competence, may require counseling periods, and in some regimes must participate in the act itself. They are constrained by the tension between their traditional commitment to preservation (duty to sustain life) and the autonomy norm (duty to respect patient choice). They also bear moral and legal liability—assisting in death exposes them to prosecution in prohibition regimes, while refusing assistance may expose them to negligence or abandonment claims in permissive regimes.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, medical_professionals, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, medical_professionals, payer).

% The state (legislature, courts, executive enforcement) in jurisdictions that prohibit physician-assisted death or medical aid in dying. They articulate and enforce the rule that neither individuals nor clinicians have authority to intentionally end a life. They defend the rule as protecting vulnerable persons, preserving the integrity of medical practice, or upholding a sanctity-of-life principle. They exercise enforcement through criminal law, licensing discipline, and prosecution. The autonomy-primary reading names this institutional actor as the primary external suppressor of individual autonomy.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, state_prohibition_authority, agenda_setter,
    institutional, generational, analytical, national).

% Organizations and individuals articulating disability-justice and non-terminal-suffering critiques. They assert that the autonomy-primary frame conflates 'unbearable suffering' with disability stigma, and that extending death authority to non-terminal disabled persons under the autonomy norm amounts to coercive culling masked as choice. They are excluded from the core frame's beneficiary set (not centered in autonomy-primary rhetoric) and bear costs through policy that applies the autonomy principle to disabled-but-not-dying populations in ways they did not author. Their objections are structurally heard but not integrated into the frame's legitimacy chain.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, disability_rights_advocates, excluded,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, disability_rights_advocates, payer).

% Partners, children, parents, and relatives of the suffering individual. They may support the autonomous choice to end life, creating no extraction from them. But the constraint extracts from families who do NOT want the death to occur: they cannot prevent it under autonomy-primary authority structures, cannot relitigate the terminal prognosis, and experience the loss through a frame that centers their loved one's autonomy rather than their own relational stake or objections. Their role is observer in the formal decision (the autonomous individual decides) but payer in the aftermath (they live with the consequence).
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, surviving_family_members, payer,
    moderate, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(dignified_death__autonomy_primary, surviving_family_members, observer).

% Faith traditions, theological movements, and sanctity-of-life constituencies that hold life's value to be non-negotiable and intentional killing—even of the suffering—to violate transcendent law. They are excluded from the autonomy-primary frame's authority structure: their objections are heard as external constraint on individual choice, not as internally legitimate voices. Permissive jurisdictions implement autonomy norms that override their doctrinal objections. They bear the cost of living in a legal regime they experience as fundamentally illegitimate.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, religious_and_sanctity_communities, excluded,
    organized, generational, mobile, national).

% Bioethicists, legal scholars, and practitioners articulating relational-autonomy and distributed-authority readings. They argue autonomy-primary reading abstracts the person from their relational embeddedness and misses how death decisions are inherently shared, that genuine autonomy requires relational support structures, and that solo-individual authority produces its own coercion. They are excluded as an authoritative voice in pure autonomy-primary frameworks; their analysis informs alternative readings but does not reshape this one.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, relational_autonomy_theorists, excluded,
    analytical, generational, analytical, national).

% The bioethical and legal analytical community observing the constraint's operation across jurisdictions and monitoring how autonomy-primary authority is implemented, what safeguards emerge, and where the frame creates unexpected harms or gaps.
narrative_ontology:constraint_stakeholder(dignified_death__autonomy_primary, analytical_observer, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(dignified_death__autonomy_primary, state_prohibition_authority).
narrative_ontology:fixing_cost_class(dignified_death__autonomy_primary, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the problem of who decides the timing and manner of death in cases of terminal suffering: the autonomy-primary frame assigns decision authority to the individual themselves, preventing ad hoc determinations by family, clinicians, or state, and establishing a single reliable answer to 'who gets to choose when I die?' This is a genuine coordination function—it removes ambiguity about legitimacy.
% TRANSFER_FUNCTION: Transfers authority and control from medical institutions, state prohibition, and collective/relational actors to the individual suffering person. The constraint moves decision-making power from clinician-centered (medical preservation) or state-centered (life as non-negotiable collective asset) regimes to individual-centered regimes. It also transfers the burden and finality of the choice onto the individual: they alone author and own the decision.
% ABSENT_VOICES: Disability-rights constituencies who experience the autonomy frame as imposing a death-favorable inference on disabled suffering (conflating disability with unbearable, terminal suffering). Relational-autonomy theorists who would redistribute decision authority across the patient-family-clinician triad. Sanctity-of-life religious communities who would reject death authority altogether. These voices are structurally excluded from the autonomy-primary authority chain; they can object but cannot reshape the frame from inside it.
% DISAPPEARANCE_RATIONALE: If the autonomy-primary dignity norm and its legal codification disappeared overnight, medical practice in permissive jurisdictions would revert to clinician-centered gatekeeping or state-centered prohibition. Suffering individuals would no longer have recognized authority to choose death. The entire institutional infrastructure of medical aid in dying (eligibility assessment, capacity evaluation, clinician participation protocols) would collapse. Individuals experiencing unbearable suffering would lose a formerly available exit and would need to pursue clandestine or violent means. The world reorganizes around the location of authority.
% FOUNDING_PROBLEM: Early medical practice centered death authority entirely in clinician judgment (Hippocratic prohibition) or state law (criminal prohibition). Suffering individuals had no recognized voice in decisions that affected their dying. The founding problem was: how should dignity be honored and agency recognized when a person faces terminal suffering and wishes to choose death? The autonomy-primary reading offers an answer: dignity resides in self-determination, so the individual must have final authority.
% FOUNDING_PROBLEM_CORROBORATION: Patients and patient advocates corroborate that the founding problem persists—many experience medical gatekeeping and legal prohibition as violations of their dignity. Disability-rights advocates contest whether the 'founding problem' is correctly framed: they argue the problem is not a lack of death authority but inadequate palliative care, social support, and freedom from disability stigma. Relational-autonomy theorists contest whether solo individual authority is the right solution. Sanctity-of-life constituencies deny the founding problem's framing entirely, arguing dignity resides in life's value, not in exit authority. The founding problem is live and contested; external corroboration comes from patient testimony, not from beneficiaries alone.
narrative_ontology:disappearance_verdict(dignified_death__autonomy_primary, world_rearranges).
narrative_ontology:founding_problem_status(dignified_death__autonomy_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(dignified_death__autonomy_primary, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(dignified_death__autonomy_primary, 'none', 1).
narrative_ontology:epsilon_provenance(dignified_death__autonomy_primary, 0.52, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dignified_death__autonomy_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(dignified_death__autonomy_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(dignified_death__autonomy_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Base extractiveness (0.52) is moderate, not high, because the autonomy-primary reading does provide real coordination (who decides) and genuine dignity recognition for those whose will is honored. But extractiveness is substantial because those whose death request is denied pay an acute cost: prolonged suffering + loss of autonomous agency + the experience of their will being overridden. The spread between beneficiaries (recognized autonomy) and victims (denied autonomy) is structurally sharp. Suppression (0.71) is high because the constraint's persistence in prohibition jurisdictions depends on active suppression of individual exit attempts (legal barriers, clinician refusal, family opposition), and in permissive jurisdictions on gatekeeping authority (capacity assessment, eligibility criteria). Suppression is not as high as it would be for pure snare because the autonomy norm genuinely resonates with many patients and clinicians, creating compliance without pure coercion. Theater ratio (0.28) is moderate-low: the capacity assessment and safeguarding procedures are substantively real, not purely performative, but gatekeeping has a performative edge (the procedures often delay or deny requests in ways that look procedurally careful but operate as outcome-controlling). Accessibility collapse (0.62) reflects that once the autonomy-primary frame is understood, few alternatives remain available in permissive jurisdictions—but in prohibition jurisdictions alternatives persist (relational frames, sanctity frames, palliative-only approaches). Resistance (0.74) is high: powerful constituencies (disability rights, sanctity-of-life, medical preservation traditions) actively resist autonomy-primary expansion; legal challenges continue; and the empirical evidence about outcomes is hotly contested. The measurement series from 1975–2025 models the spreading codification of autonomy-primary norms: extractiveness has risen as the reading gains legal authority (suffering individuals' exit requests are more often honored, which would seem to reduce extraction, but gatekeeping and eligibility criteria have also tightened, creating the net modest rise). Suppression has fallen as prohibition has eroded in permissive jurisdictions, but remains high where prohibition is maintained. Theater has risen as institutional procedures have matured.
 *
 * PERSPECTIVAL GAP:
 *   From the autonomy-primary seat, the constraint is genuine coordination: 'who decides?' is answered clearly ('I do'), dignity is protected, and individual will is respected. From the sanctity-primary seat, the same structure is extraction: an illegitimate authority to kill is imposed, individual preference usurps transcendent law, and dignity is violated by death itself. From the relational-autonomy seat, both are incompletely framed: true autonomy requires relational support and distributed authority, solo individual choice is isolating, and the procedure lacks the oversight and relational embedding genuine autonomy requires. These perspectival gaps are not resolvable by better information; they rest on different foundational premises about what dignity is. The engine computes the per-seat type from the structural relationships, not from reconciling the perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiary/victim declarations follow from the autonomy-primary reading itself: the reading declares that autonomy resides in the individual, so individuals who exercise autonomous choice are beneficiaries and those denied choice are victims. The directionality flow is: beneficiary (autonomy honored) gets d near 0.0 (subsidy/protection), victim (autonomy denied) gets d near 1.0 (extraction/suppression). Medical gatekeepers and state authorities hold ambiguous d because they are simultaneously implementers of the autonomy norm (in permissive jurisdictions) and suppressors of it (in prohibition regimes). Disability-rights advocates are harmed by the misapplication of the frame to non-terminal suffering, creating extraction through coercive inference, so their d is high. The directionality is stable across the interval; what changes is the distribution of agents across the seats as more jurisdictions adopt autonomy-primary codification.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint does not show mandatrophy (founding problem dead, function atrophied) in most jurisdictions. The founding problem (who decides?) remains live. The coordination function (clarifying authority, enabling choice) remains real. In prohibition jurisdictions, the autonomy-primary norm persists as a powerful discursive force even though it lacks legal codification—it is not theatrical or inert. In permissive jurisdictions, the norm is actively implemented through institutional procedures (capacity assessment, counseling, clinician coordination). However, a mandatrophy hypothesis is worth examining: in some permissive jurisdictions, the autonomy norm may have decoupled from the actual lived experience of individuals—the procedures may have become gatekeeping theater that frustrates the very autonomy they claim to protect. If individuals systematically experience gatekeeping delays and denials despite satisfying statutory criteria, the founding problem (who decides?) is functionally dead (individuals do not in fact decide) but the norm persists in rhetoric. This would be mandatrophy: the function has atrophied but the constraint persists through institutional inertia. The measurement of theater_ratio rising from 0.05 to 0.28 suggests growing performativity—capacity assessment and waiting periods that look procedurally careful but operate as outcome-controlling. This is not mandatrophy yet (the coordination function remains real), but it flags the risk.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    autonomy_vs_sanctity_foreclosure,
    'Does the autonomy-primary axiom logically foreclose the sanctity-primary axiom within a single coherent legal/ethical framework, or can both be held as competing live positions?',
    'Examine jurisdictions that attempt to hold both: does the legal/institutional attempt produce internal contradiction (forcing courts to apply incompatible rules) or does it produce stable coexistence via domain-splitting (autonomy in some cases, sanctity constraints in others)? If coexistence is stable, they coexist_with; if contradiction is irresolvable, one forecloses the other.',
    'If forecloses, the reading relation shifts to forecloses and the axioms are structurally incompatible. If coexists_with, the framework is genuinely plural and both readings remain live even within single jurisdictions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(autonomy_vs_sanctity_foreclosure, conceptual, 'Whether autonomy-primary and sanctity-primary axioms logically foreclose each other or can coexist as competing frameworks.').

omega_variable(
    unbearable_suffering_operationalization,
    'What criteria operationalize ''unbearable suffering'' such that individual authority is triggered? Is ''unbearable'' a subjective judgment (patient''s own report) or objective assessment (clinician''s diagnosis of pain/prognosis)?',
    'Audit of medical aid in dying jurisdictions'' statutory definitions and case law: does the operationalization privilege subjective report (high extraction from those denied exit) or objective criteria (higher suppression through medical gatekeeping)? Map the shift over time as regimes mature.',
    'High dependence on subjective report amplifies individual autonomy but lowers suppression (more people gain exit). High dependence on objective criteria amplifies suppression (medical gatekeeping) and increases extraction from those denied exit via clinical judgment. The measured extractiveness and suppression are sensitive to this operationalization.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(unbearable_suffering_operationalization, empirical, 'How ''unbearable suffering'' is defined operationally, and whether the definition is subjective or objective.').

omega_variable(
    relational_autonomy_alternative_framing,
    'If decision authority were distributed across patient-family-clinician triad with procedural safeguards (the relational-autonomy sibling), would the same individuals experience this as autonomy violation or as enhanced protection?',
    'Empirical study comparing patient experience in solo-autonomy regimes vs. relational-autonomy regimes: do patients report greater dignity/autonomy in solo-authority, or do they report feeling unsupported/isolated? Post-decision regret and satisfaction data.',
    'If patients in relational regimes report enhanced autonomy and dignity, the autonomy-primary reading may be misnamed—autonomy may not be what it claims to protect. If patients report enhanced support and sustained choice-ownership, the relational reading legitimacy increases. If patients in both regimes report similar outcomes, the reading difference is more about institutional power allocation than individual experience.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(relational_autonomy_alternative_framing, empirical, 'Whether patient autonomy experience differs between solo-authority and relational-distributed-authority regimes.').

omega_variable(
    disability_stigma_conflation,
    'Does the autonomy-primary frame''s expansion to non-terminal disabled suffering embed an implicit inference that disabled life is inherently ''unbearable,'' creating a coercive context for disabled persons to exercise ''autonomy'' toward death?',
    'Track jurisdictions that extend autonomy to psychological suffering or disability-related suffering (not terminal): map rates of death requests among disabled vs. non-disabled, and measure qualitative reports from disabled deciders about whether social isolation, economic abandonment, or lack of accommodation constrained their choice.',
    'If the inference is embedded and coercive, the autonomy-primary frame extracts from disabled suffering individuals not by denying exit but by creating structural pressure to exit. The measured extractiveness would need to account for this hidden extraction. The victim set would expand beyond those denied exit to include those whose exit is coerced by stigma.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(disability_stigma_conflation, empirical, 'Whether autonomy-primary frame''s expansion to non-terminal suffering embeds coercive inference that disabled life is unbearable.').

omega_variable(
    kernel_reading_committer_structure,
    'This constraint is ONE reading of the dignified_death kernel. The sibling readings autonomy_primary, relational_autonomy, and sanctity_primary each instantiate separate constraints with different ε values, beneficiary/victim structures, and types. The contest among readings is the dignified_death kernel dispute. Is the contest empirically resolvable (one reading is more consistent, grounded, or effective) or is it fundamentally incommensurable (different foundational premises about the nature of dignity)?',
    'Systematic comparison across jurisdictions: (1) Empirical outcomes data—do autonomy-primary regimes produce the dignity outcomes they claim? Do sanctity regimes protect life effectively? Do relational regimes support genuine autonomy? (2) Longitudinal discourse analysis—do proponents of each reading attempt unified theoretical frameworks (subsumption of rivals) or explicitly acknowledge incommensurability? (3) Patient/family experience studies—which reading''s implementation produces the fewest regrets and highest reported dignity?',
    'If one reading is empirically superior, that reading''s ε and type are stable while alternatives may require recalibration as losing ground. If readings remain incommensurable despite evidence, each reading''s ε is stable relative to its own reference frame, but cross-reading comparison requires frame-fixing—there is no single ''correct'' dignified_death constraint, only reading-indexed constraints that cannot be unified. The engine computes per-reading type; divergence among readings reflects genuine structural incommensurability in the kernel, not measurement error.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_committer_structure, conceptual, 'Whether the three kernel readings are logically incommensurable or resolvable to a single superior framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dignified_death__autonomy_primary, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dign_tr_t1975, dignified_death__autonomy_primary, theater_ratio, 1975, 0.05).
narrative_ontology:measurement_basis(dign_tr_t1975, projected).
narrative_ontology:measurement(dign_tr_t1990, dignified_death__autonomy_primary, theater_ratio, 1990, 0.08).
narrative_ontology:measurement_basis(dign_tr_t1990, observed).
narrative_ontology:measurement(dign_tr_t2005, dignified_death__autonomy_primary, theater_ratio, 2005, 0.15).
narrative_ontology:measurement_basis(dign_tr_t2005, observed).
narrative_ontology:measurement(dign_tr_t2015, dignified_death__autonomy_primary, theater_ratio, 2015, 0.22).
narrative_ontology:measurement_basis(dign_tr_t2015, observed).
narrative_ontology:measurement(dign_tr_t2020, dignified_death__autonomy_primary, theater_ratio, 2020, 0.26).
narrative_ontology:measurement_basis(dign_tr_t2020, observed).
narrative_ontology:measurement(dign_tr_t2025, dignified_death__autonomy_primary, theater_ratio, 2025, 0.28).
narrative_ontology:measurement_basis(dign_tr_t2025, observed).

% Extraction over time
narrative_ontology:measurement(dign_be_t1975, dignified_death__autonomy_primary, base_extractiveness, 1975, 0.12).
narrative_ontology:measurement_basis(dign_be_t1975, projected).
narrative_ontology:measurement(dign_be_t1990, dignified_death__autonomy_primary, base_extractiveness, 1990, 0.24).
narrative_ontology:measurement_basis(dign_be_t1990, observed).
narrative_ontology:measurement(dign_be_t2005, dignified_death__autonomy_primary, base_extractiveness, 2005, 0.38).
narrative_ontology:measurement_basis(dign_be_t2005, observed).
narrative_ontology:measurement(dign_be_t2015, dignified_death__autonomy_primary, base_extractiveness, 2015, 0.46).
narrative_ontology:measurement_basis(dign_be_t2015, observed).
narrative_ontology:measurement(dign_be_t2020, dignified_death__autonomy_primary, base_extractiveness, 2020, 0.5).
narrative_ontology:measurement_basis(dign_be_t2020, observed).
narrative_ontology:measurement(dign_be_t2025, dignified_death__autonomy_primary, base_extractiveness, 2025, 0.52).
narrative_ontology:measurement_basis(dign_be_t2025, observed).

% Suppression requirement over time
narrative_ontology:measurement(dign_su_t1975, dignified_death__autonomy_primary, suppression_requirement, 1975, 0.88).
narrative_ontology:measurement_basis(dign_su_t1975, projected).
narrative_ontology:measurement(dign_su_t1990, dignified_death__autonomy_primary, suppression_requirement, 1990, 0.82).
narrative_ontology:measurement_basis(dign_su_t1990, observed).
narrative_ontology:measurement(dign_su_t2005, dignified_death__autonomy_primary, suppression_requirement, 2005, 0.76).
narrative_ontology:measurement_basis(dign_su_t2005, observed).
narrative_ontology:measurement(dign_su_t2015, dignified_death__autonomy_primary, suppression_requirement, 2015, 0.73).
narrative_ontology:measurement_basis(dign_su_t2015, observed).
narrative_ontology:measurement(dign_su_t2020, dignified_death__autonomy_primary, suppression_requirement, 2020, 0.72).
narrative_ontology:measurement_basis(dign_su_t2020, observed).
narrative_ontology:measurement(dign_su_t2025, dignified_death__autonomy_primary, suppression_requirement, 2025, 0.71).
narrative_ontology:measurement_basis(dign_su_t2025, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dignified_death__autonomy_primary, resource_allocation).
narrative_ontology:boltzmann_floor_override(dignified_death__autonomy_primary, 0.12).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__sanctity_primary).
narrative_ontology:affects_constraint(dignified_death__autonomy_primary, dignified_death__relational_autonomy).

% DUAL FORMULATION NOTE:
% The dignified_death kernel admits three structurally distinct readings, each instantiated as a separate constraint. autonomy_primary (this constraint) asserts dignity resides in individual self-determination; sanctity_primary asserts dignity resides in life's intrinsic value; relational_autonomy asserts dignity emerges from relational context and distributed authority. Each reading has different ε (autonomy_primary 0.52, sanctity_primary ~0.30, relational_autonomy ~0.42), different beneficiary/victim structures, different types. They do not coexist peacefully within single jurisdictions—legal codification of one reading displaces the others into excluded voices. The three constraints are linked by network.affects_constraints; the sibling readings are documented in cs_structure.reading_relations and cs_structure.axioms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
