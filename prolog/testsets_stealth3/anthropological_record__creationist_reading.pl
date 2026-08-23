% ============================================================================
% CONSTRAINT STORY: anthropological_record__creationist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_anthropological_record__creationist_reading, []).

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
 *   constraint_id: anthropological_record__creationist_reading
 *   human_readable: Creationist Reading Requirement for the Anthropological Record
 *   domain: epistemological/religious/institutional
 *
 * SUMMARY:
 *   Within religious communities that adopt it, the creationist reading
 *   operates as an enforced interpretive constraint on the anthropological
 *   record: the record must be read as revealing divine creation - compatible
 *   with the scriptural timeline or with designed complexity - materialist
 *   timelines are suppressed, and credentialed science loses its adjudicative
 *   standing. The constraint has a genuine coordination function (a shared
 *   origin narrative that binds identity, moral formation, and
 *   intergenerational continuity) and a real extraction profile (epistemic
 *   closure, vocational foreclosure for the science-inclined, discipline for
 *   dissenters, and a transfer of adjudicative authority and revenue to
 *   doctrinal authorities and creationist institutions). The claimed type is
 *   rope: the reading's own self-framing, in which covenantal coordination on
 *   truth suppresses no legitimate alternative because the alternative is
 *   simply error. The authored metrics describe the enforced, asymmetric
 *   operation; the engine computes per-seat classifications from the
 *   structural data, and that divergence is the measurement. This file is one
 *   reading of the anthropological_record kernel; the naturalist and
 *   indigenous-epistemology siblings are separate constraints with their own
 *   referents and epsilon values (see network.dual_formulation_note).
 *
 * KEY AGENTS:
 *   - clergy_and_doctrinal_authorities: agenda-setter and primary beneficiary (institutional / identity_locked) - adjudicate the reading, control teaching and discipline, capture interpretive authority
 *   - creationist_educational_institutions: beneficiary (institutional / identity_locked) - schools, museums, and media whose revenue and purpose the mandatory reading sustains
 *   - believing_community_members: coordinated members, dual beneficiary/payer (organized / identity_locked) - receive identity and meaning, pay epistemic and vocational costs
 *   - science_inclined_youth: primary target (powerless / trapped) - taught the reading as fact, vocation foreclosed at the community boundary
 *   - credentialed_scientists: excluded adjudicators (institutional / mobile) - bear loss of standing inside the community; mobile exit outside it
 *   - dissenting_believers: internal targets (moderate / identity_locked) - hold heterodox readings, pay in suspicion and silence
 *   - philosophy_of_science_scholars: analytical observer - sees the full structure without a ruling seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(anthropological_record__creationist_reading, 0.58).
domain_priors:suppression_score(anthropological_record__creationist_reading, 0.62).
domain_priors:theater_ratio(anthropological_record__creationist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(anthropological_record__creationist_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(anthropological_record__creationist_reading, rope).
narrative_ontology:human_readable(anthropological_record__creationist_reading, "Creationist Reading Requirement for the Anthropological Record").
narrative_ontology:topic_domain(anthropological_record__creationist_reading, "epistemological/religious/institutional").

domain_priors:requires_active_enforcement(anthropological_record__creationist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(anthropological_record__creationist_reading, '2eafa082-6ab6-4522-a94a-a5853e7a39ff').
narrative_ontology:cs_kernel_codification('2eafa082-6ab6-4522-a94a-a5853e7a39ff', fixed_text).
narrative_ontology:cs_authority_grounding('2eafa082-6ab6-4522-a94a-a5853e7a39ff', lineage).
narrative_ontology:cs_interpretation_layer_present('2eafa082-6ab6-4522-a94a-a5853e7a39ff').
narrative_ontology:cs_reading_relation('2eafa082-6ab6-4522-a94a-a5853e7a39ff', anthropological_record__naturalist_reading, forecloses).
narrative_ontology:cs_reading_relation('2eafa082-6ab6-4522-a94a-a5853e7a39ff', anthropological_record__indigenous_epistemology_reading, coexists_with).
narrative_ontology:cs_axiom('2eafa082-6ab6-4522-a94a-a5853e7a39ff', foundational, divine_causation_required_in_origin_accounts).
narrative_ontology:cs_axiom_status(divine_causation_required_in_origin_accounts, holdable).
narrative_ontology:cs_axiom_grounding('2eafa082-6ab6-4522-a94a-a5853e7a39ff', divine_causation_required_in_origin_accounts, theological).
narrative_ontology:cs_axiom('2eafa082-6ab6-4522-a94a-a5853e7a39ff', foundational, scriptural_text_adjudicates_origin_claims).
narrative_ontology:cs_axiom_status(scriptural_text_adjudicates_origin_claims, holdable).
narrative_ontology:cs_axiom_grounding('2eafa082-6ab6-4522-a94a-a5853e7a39ff', scriptural_text_adjudicates_origin_claims, theological).
narrative_ontology:cs_reference_frame('2eafa082-6ab6-4522-a94a-a5853e7a39ff', scripture_keyed_revelatory_record).
narrative_ontology:cs_drift_state('2eafa082-6ab6-4522-a94a-a5853e7a39ff', contemporary, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('2eafa082-6ab6-4522-a94a-a5853e7a39ff', '').
narrative_ontology:cs_kernel_id(anthropological_record__creationist_reading, anthropological_record).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, clergy_and_doctrinal_authorities).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, creationist_educational_institutions).
narrative_ontology:constraint_beneficiary(anthropological_record__creationist_reading, believing_community_members).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, science_inclined_youth).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, credentialed_scientists).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, dissenting_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(anthropological_record__creationist_reading, believing_community_members).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, biblical_inerrancy_doctrine).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, designed_complexity_hypothesis).
narrative_ontology:constraint_vindicates(anthropological_record__creationist_reading, recent_creation_chronology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Ordain, teach, and discipline within the community. They author the doctrinal statements that require the creationist reading, control pulpits, seminaries, and curricula, and their standing as the authorized interpreters of the record depends on the constraint holding. Leaving would cost them vocation, community, and the identity their office constitutes.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, clergy_and_doctrinal_authorities, agenda_setter,
    institutional, generational, identity_locked, national).

% Operate schools, museums, and media ministries whose enrollment, donations, admissions, and institutional purpose depend on the reading being mandatory. They produce the curricula and exhibits that stage the reading in scientific form. Repurposing would dissolve the institution's reason for existing.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, creationist_educational_institutions, beneficiary,
    institutional, generational, identity_locked, national).

% Receive the shared origin narrative that organizes their identity, moral formation, and community belonging. They also comply: they affirm the reading in membership covenants, fund its institutions, and absorb the epistemic costs - science education that stops at the community's boundary, and the private management of doubt. Leaving would cost family, community, and self-understanding at once.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, believing_community_members, beneficiary,
    organized, biographical, identity_locked, regional).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, believing_community_members, payer).

% Raised inside the reading and taught it as fact. Those drawn to geology, biology, or anthropology hit the community's boundary exactly at their vocation. Dependent on family and congregation, they cannot exit until adulthood, and exit then means losing the community rather than revising the reading.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, science_inclined_youth, payer,
    powerless, biographical, trapped, local).

% Hold the adjudicative standing the constraint removes. Within the community their findings carry no ruling force and their account of the record is treated as error or deception. They work outside the community and lose little materially; the cost they bear is the standing itself, and the members' lost access to what they know.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, credentialed_scientists, payer,
    institutional, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(anthropological_record__creationist_reading, credentialed_scientists, excluded).

% Remain inside the community while holding evolutionary or old-earth readings. They teach Sunday school with care, hedge in small groups, or go quiet. They want the community and the science both, and the constraint prices that combination in suspicion, discipline, or silence.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, dissenting_believers, payer,
    moderate, biographical, identity_locked, regional).

% Study how communities adjudicate origin claims and what demarcates science from covenant. They take testimony from every seat without holding a ruling position in any community.
narrative_ontology:constraint_stakeholder(anthropological_record__creationist_reading, philosophy_of_science_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(anthropological_record__creationist_reading, clergy_and_doctrinal_authorities).
narrative_ontology:fixing_cost_class(anthropological_record__creationist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves, once and communally, the problem of giving every member the same answer to where humans came from: one authorized reading, transmitted through pulpit and school, binds the community's identity, moral order, and intergenerational continuity, and spares members the burden of adjudicating origins alone against credentialed specialists.
% TRANSFER_FUNCTION: Moves interpretive authority over the record from credentialed science and from members' private judgment to the community's doctrinal authorities; moves money (tuition, donations, admissions) and status to the creationist institutional complex; moves intellectual autonomy and, for the young, vocational options from members to the arrangement.
% ABSENT_VOICES: Credentialed scientists stand outside the boundary the constraint draws - their objection is precisely what the arrangement exists to make non-adjudicative. Dissenting believers and doubting youth are present but muted; theistic-evolutionist teachers hedge or leave. The unanimity the community displays is real inside the boundary and manufactured at it.
% DISAPPEARANCE_RATIONALE: If the requirement vanished overnight, creationist schools and museums would lose their warrant within a generation as curricula converged on mainstream science, doctrinal authorities would lose the adjudicative standing the constraint confers, and members' relationship to scientific training would reopen. The communities would not dissolve, but their educational economy, their boundary against secular institutions, and their authorities' office would all reorganize.
% FOUNDING_PROBLEM: How a community whose identity, moral order, and scriptural authority rest on a divine-creation account should respond when credentialed science offers a materialist origin account that appears to falsify that foundation - the late-nineteenth and early-twentieth-century authority crisis out of which the modern reading crystallized.
% FOUNDING_PROBLEM_CORROBORATION: Historians of the fundamentalist-modernist controversy, writing from outside the benefiting parties, attest that the reading crystallized as a response to the scriptural-authority crisis of roughly 1890-1930. Dissenting believers inside the communities and ex-member testimony attest the problem remains live for members who experience the choice between community and science as real.
narrative_ontology:disappearance_verdict(anthropological_record__creationist_reading, world_rearranges).
narrative_ontology:founding_problem_status(anthropological_record__creationist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(anthropological_record__creationist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(anthropological_record__creationist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(anthropological_record__creationist_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(anthropological_record__creationist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(anthropological_record__creationist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(anthropological_record__creationist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate-high (0.58 at interval end) because the constraint's costs and benefits are asymmetric: members fund and comply, authorities and institutions capture standing and revenue, and the epistemic costs (truncated science education, managed doubt) fall on those with least say. Suppression (0.62) is structural - doctrinal statements, curriculum control, employment covenants, church discipline - layered over internalized self-censorship. Theater is moderate (0.34): the identity-formation function is genuinely performed, while a growing share of institutional activity stages scientific form (journals, museums, the debate circuit) rather than performing it. Accessibility collapse is 0.50: inside the community's frame the naturalist alternative loses standing, but it remains visibly practiced outside, so alternatives never fully collapse. Resistance is 0.55: deconversion, heterodox movements, and public contestation are real and ongoing. The three series share one time grid (t=0 corresponds to 1905, t=120 to 2025) so every metric is authored at every examined point. The suppression_requirement series is authored because this story specifically tracks enforcement-machinery build-up: loose denominational latitude (1905), confessional tests and seminary purges in the fundamentalist controversies (1920s-1940s), professionalized creation science with signed doctrinal statements (1960s-1980s), peak covenant enforcement around the museum-building era (2000s), and partial relaxation under online exposure (2020s). The slight end-of-interval softening in all three series reflects younger-cohort drift toward evolutionary-creation accommodation, not resolution.
 *
 * PERSPECTIVAL GAP:
 *   The agenda-setter seat computes a coordination experience: from the doctrinal authorities' position the constraint is the community's covenantal spine, and its enforcement is fidelity. The payer seats compute differently. A science-inclined youth experiences vocational foreclosure at the boundary of the only community they have; a dissenting believer experiences discipline for holding what credentialed science holds; credentialed scientists experience the removal of their standing as adjudicators - though their mobile exit damps the extraction that lands on them personally, shifting its weight onto members who cannot exit. Same-power seats also diverge: clergy and credentialed scientists both hold institutional power, but the constraint transfers standing from one to the other, so identical global power produces opposite directionalities. The engine computes this divergence from the structural data; the authored rope claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Doctrinal authorities and creationist institutions are structural beneficiaries - the constraint confers their standing and their revenue - so they derive low d and low effective extraction. Believing members are dual-positioned: they appear in both the beneficiary and victim declarations, and their identity-locked exit pushes them toward the target end while their genuine identity and meaning gains pull the other way. A directionality override sets the organized power atom near-symmetric (d = 0.5) because the derivation cannot weigh the two sides for the only seat holding it. Science-inclined youth and dissenting believers derive near-full-target directionality: trapped or identity-locked, they bear the constraint's costs with no arbitrage. Credentialed scientists are declared victims - the constraint removes their adjudicative standing - but their mobile exit means effective extraction is damped for their seat; the extraction they escape lands on the members who stay.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - how a scriptural-authority community answers a materialist origin science - is live, so the constraint is not mandatrophy-resolved: its enforcement still performs identity work the community would otherwise lack. The classification prevents two mislabels. Reading the constraint as pure rope launders the extraction: the identity frame is genuine but is also the cover under which authorities capture adjudicative standing and institutions capture revenue. Reading it as pure snare erases the coordination: members demonstrably receive meaning, moral formation, and belonging they do not trade cheaply. The structural data shows genuine coordination and asymmetric extraction through the same enforced structure, even though the reading itself claims rope; the per-seat computation keeps the self-framing from settling the question. If the founding problem ever resolves through broad accommodation of mainstream science inside these communities, the enforcement machinery would become transitional and need a declared sunset; the interpretive layer's accommodations (day-age readings, old-earth creationism, intelligent design) are the visible leading edge of that drift.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is the creationist reading of the anthropological_record kernel; the naturalist and indigenous-epistemology readings instantiate different constraints over the same record. Is the live disagreement located in the record''s content (timeline, causation) or in adjudicative authority (who may rule on origin claims)?',
    'Comparative analysis across the three sibling stories: locate each reading''s contested element and test whether the disputes converge on the record''s content or on interpretive standing.',
    'If the dispute is primarily about adjudicative authority, the constraint''s extraction profile is dominated by the authority transfer rather than the timeline claims, and remedies aimed at content (evidential debate) miss the structure entirely.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Committer structure: this story is one reading of a contested kernel; where the sibling disagreement is located.').

omega_variable(
    identity_frame_cover_question,
    'Is the identity frame a genuine coordination good that happens to carry costs, or primarily the cover under which authorities and institutions capture adjudicative standing and revenue?',
    'Compare member welfare trajectories and exit patterns in communities holding the constraint against matched communities with relaxed readings; measure who captures the constraint''s revenue and standing flows.',
    'If capture dominates, classification moves toward snare; if identity goods dominate and the costs track genuine formation, the rope claim strengthens.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_frame_cover_question, empirical, 'Whether the identity-coordination frame is genuine coordination or extraction cover.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (doctrinal statements, curriculum control, employment covenants, church discipline) or internalized (identity-fused self-censorship that would persist if the enforcement machinery were removed)?',
    'Post-exit suppression trajectory of leavers, and survey of doubting members in communities that relaxed enforcement: if doubt-management and self-censorship persist after the machinery is removed, the internalized share is substantial.',
    'If internalized, effective suppression exceeds the structural measure - members carry the constraint''s boundary with them after exit, and enforcement decay would relax the constraint less than the suppression series suggests.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized suppression mechanism.').

omega_variable(
    member_net_outcomes_ambiguity,
    'Does credentialed science''s loss of adjudicative standing impose net costs on members (truncated science education, foreclosed vocations, managed doubt) or net protection (from what the community reads as materialist metaphysics exceeding the evidence)?',
    'Longitudinal tracking of educational, vocational, and welfare outcomes for members educated under the constraint versus matched communities with relaxed readings, with ex-member interviews supplying the counterfactual.',
    'Net costs would confirm the victim declarations as extraction from members; net protection would shift weight toward the coordination reading and reposition the scientist seat as boundary-relevant rather than victim-relevant.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(member_net_outcomes_ambiguity, empirical, 'Whether the adjudicative-monopoly transfer costs or protects members.').

omega_variable(
    kernel_location_ambiguity,
    'Is the stabilized kernel the scriptural text itself, or the doctrinal authorities'' monopoly over its interpretation? The obvious framing (Scripture as kernel) and the less obvious one (the interpretive monopoly as kernel) produce different classifications.',
    'Test whether the community tolerates scripture-faithful readings that bypass authorized interpreters (private readings contradicting doctrinal statements): if those are also suppressed, the operative kernel is the interpretive monopoly, not the text.',
    'If the interpretive monopoly is the kernel, the constraint reads as authority-protection rather than revelation-protection, raising effective extraction and pushing the classification from tangled coordination toward capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_location_ambiguity, conceptual, 'CS-framing under-determination: text-as-kernel versus interpretive-monopoly-as-kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(anthropological_record__creationist_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(anth_tr_t0, anthropological_record__creationist_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement_basis(anth_tr_t0, observed).
narrative_ontology:measurement(anth_tr_t20, anthropological_record__creationist_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement_basis(anth_tr_t20, observed).
narrative_ontology:measurement(anth_tr_t40, anthropological_record__creationist_reading, theater_ratio, 40, 0.22).
narrative_ontology:measurement_basis(anth_tr_t40, observed).
narrative_ontology:measurement(anth_tr_t60, anthropological_record__creationist_reading, theater_ratio, 60, 0.28).
narrative_ontology:measurement_basis(anth_tr_t60, observed).
narrative_ontology:measurement(anth_tr_t80, anthropological_record__creationist_reading, theater_ratio, 80, 0.33).
narrative_ontology:measurement_basis(anth_tr_t80, observed).
narrative_ontology:measurement(anth_tr_t100, anthropological_record__creationist_reading, theater_ratio, 100, 0.36).
narrative_ontology:measurement_basis(anth_tr_t100, observed).
narrative_ontology:measurement(anth_tr_t120, anthropological_record__creationist_reading, theater_ratio, 120, 0.34).
narrative_ontology:measurement_basis(anth_tr_t120, observed).

% Extraction over time
narrative_ontology:measurement(anth_be_t0, anthropological_record__creationist_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement_basis(anth_be_t0, observed).
narrative_ontology:measurement(anth_be_t20, anthropological_record__creationist_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(anth_be_t20, observed).
narrative_ontology:measurement(anth_be_t40, anthropological_record__creationist_reading, base_extractiveness, 40, 0.46).
narrative_ontology:measurement_basis(anth_be_t40, observed).
narrative_ontology:measurement(anth_be_t60, anthropological_record__creationist_reading, base_extractiveness, 60, 0.52).
narrative_ontology:measurement_basis(anth_be_t60, observed).
narrative_ontology:measurement(anth_be_t80, anthropological_record__creationist_reading, base_extractiveness, 80, 0.57).
narrative_ontology:measurement_basis(anth_be_t80, observed).
narrative_ontology:measurement(anth_be_t100, anthropological_record__creationist_reading, base_extractiveness, 100, 0.6).
narrative_ontology:measurement_basis(anth_be_t100, observed).
narrative_ontology:measurement(anth_be_t120, anthropological_record__creationist_reading, base_extractiveness, 120, 0.58).
narrative_ontology:measurement_basis(anth_be_t120, observed).

% Suppression requirement over time
narrative_ontology:measurement(anth_su_t0, anthropological_record__creationist_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement_basis(anth_su_t0, observed).
narrative_ontology:measurement(anth_su_t20, anthropological_record__creationist_reading, suppression_requirement, 20, 0.48).
narrative_ontology:measurement_basis(anth_su_t20, observed).
narrative_ontology:measurement(anth_su_t40, anthropological_record__creationist_reading, suppression_requirement, 40, 0.52).
narrative_ontology:measurement_basis(anth_su_t40, observed).
narrative_ontology:measurement(anth_su_t60, anthropological_record__creationist_reading, suppression_requirement, 60, 0.58).
narrative_ontology:measurement_basis(anth_su_t60, observed).
narrative_ontology:measurement(anth_su_t80, anthropological_record__creationist_reading, suppression_requirement, 80, 0.64).
narrative_ontology:measurement_basis(anth_su_t80, observed).
narrative_ontology:measurement(anth_su_t100, anthropological_record__creationist_reading, suppression_requirement, 100, 0.66).
narrative_ontology:measurement_basis(anth_su_t100, observed).
narrative_ontology:measurement(anth_su_t120, anthropological_record__creationist_reading, suppression_requirement, 120, 0.62).
narrative_ontology:measurement_basis(anth_su_t120, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(anthropological_record__creationist_reading, identity_coordination).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__naturalist_reading).
narrative_ontology:affects_constraint(anthropological_record__creationist_reading, anthropological_record__indigenous_epistemology_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'what the anthropological record reveals' decomposes into three structurally distinct constraints - the creationist reading (this file), the naturalist reading, and the indigenous-epistemology reading - each with its own epsilon, beneficiary structure, and adjudicative arrangement. The readings share a referent kernel (the record) but instantiate different constraints: this story's epsilon is authored for the creationist interpretive regime by the creationist reading's own lights; the naturalist sibling authors epsilon for the scientific-adjudication regime it instantiates. The naturalist sibling is upstream in practice: its institutional ascendancy is the challenge that hardened this reading's enforcement across 1920-2010, which is why this file links to it. Constraint-family membership: every story in the anthropological_record family links at least one sibling.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(anthropological_record__creationist_reading, organized, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
