% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_refugee_convention_text__expansive_humanitarian_reading, []).

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
 *   constraint_id: refugee_convention_text__expansive_humanitarian_reading
 *   human_readable: Refugee Convention as Expansive Humanitarian Mandate (Broad Protection Reading)
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   The 1951 Refugee Convention and its 1967 Protocol are foundational
 *   instruments in international refugee law, yet they remain deeply
 *   contested in interpretation and application. This constraint instantiates
 *   ONE READING of the Convention's text: the expansive humanitarian reading,
 *   which interprets 'well-founded fear of persecution' to encompass
 *   generalized violence and non-state persecution, treats 'particular social
 *   group' to include gender, LGBTQ+ identity, and clan-based persecution,
 *   and holds the principle of non-refoulement ('shall not return') to
 *   prohibit indirect exclusion (interdiction, offshore processing) in
 *   addition to direct return. Under this reading, the Convention operates as
 *   an unbendable humanitarian mandate requiring broad protection,
 *   substantive assessment of all claims, and internalization of asylum
 *   responsibility rather than externalization. The constraint is NOT the
 *   text itself (which is static) but the reading's OPERATION AS A BINDING
 *   MANDATE in governance and adjudication — how states implement, judges
 *   interpret, and advocates claim rights under this particular reading. The
 *   preamble's humanitarian purpose is the reading's foundational
 *   hermeneutical canon; state practice and refugee advocacy organizations
 *   adopt and enforce this interpretation in testimony, litigation, and
 *   implementation. Sibling readings (restrictive sovereignty reading,
 *   procedural integrity reading) read the same text differently, generating
 *   different constraints with different victim sets, different costs, and
 *   different terminal classifications. This story instantiates only the
 *   expansive humanitarian reading; the others are separate constraint
 *   stories, linked via the network.
 *
 * KEY AGENTS:
 *   - asylum_claimants_broad_cohort: individuals fleeing generalized violence, gender-based persecution, LGBTQ+ persecution, clan-based persecution — the expanded victim set under this reading
 *   - human_rights_advocacy_organizations: UNHCR, international refugee law NGOs, regional human rights bodies (ECtHR, IACtHR) — defend and propagate the expansive reading in litigation and advisory opinions
 *   - signatory_states: 196 parties to the Convention — implement the text; divide between those interpreting broadly (many European states, Canada) and narrowly (Australia, US under restrictive administrations, Denmark)
 *   - origin_states_and_non_state_actors: governments and armed groups in countries generating asylum flows — define the persecution mechanisms (generalized violence, gender-based violence, clan warfare) that the reading's victim category encompasses
 *   - domestic_adjudicators: immigration judges, asylum appeal boards, courts — apply the reading in individual cases; determine whether a claimant falls within the broad or narrow interpretation
 *   - analytical_observer: the interpretive community (academic international law, UNHCR legal analysis) — identifies the reading as one among several coherent interpretations
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.28).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.22).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.28).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.71).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention as Expansive Humanitarian Mandate (Broad Protection Reading)").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, 'd7071972-7d6f-4d26-a13e-2790fdf3ae69').
narrative_ontology:cs_kernel_codification('d7071972-7d6f-4d26-a13e-2790fdf3ae69', fixed_text).
narrative_ontology:cs_authority_grounding('d7071972-7d6f-4d26-a13e-2790fdf3ae69', lineage).
narrative_ontology:cs_interpretation_layer_present('d7071972-7d6f-4d26-a13e-2790fdf3ae69').
narrative_ontology:cs_reading_relation('d7071972-7d6f-4d26-a13e-2790fdf3ae69', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('d7071972-7d6f-4d26-a13e-2790fdf3ae69', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('d7071972-7d6f-4d26-a13e-2790fdf3ae69', foundational, persecution_protection_transcends_sovereignty).
narrative_ontology:cs_axiom_status(persecution_protection_transcends_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d7071972-7d6f-4d26-a13e-2790fdf3ae69', persecution_protection_transcends_sovereignty, deontological).
narrative_ontology:cs_axiom('d7071972-7d6f-4d26-a13e-2790fdf3ae69', foundational, generalized_violence_constitutes_persecution).
narrative_ontology:cs_axiom_status(generalized_violence_constitutes_persecution, holdable).
narrative_ontology:cs_axiom_grounding('d7071972-7d6f-4d26-a13e-2790fdf3ae69', generalized_violence_constitutes_persecution, empirically_contingent).
narrative_ontology:cs_reference_frame('d7071972-7d6f-4d26-a13e-2790fdf3ae69', humanitarian_protection_universal_mandate).
narrative_ontology:cs_drift_state('d7071972-7d6f-4d26-a13e-2790fdf3ae69', contemporary_2020s_asylum_pressures, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('d7071972-7d6f-4d26-a13e-2790fdf3ae69', '2026-06-11T14:32:00Z').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_claimants_broad_cohort).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, human_rights_advocacy_organizations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, signatory_states_restrictive_interpretation_coalition).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Individuals fleeing generalized violence, gender-based persecution, LGBTQ+ persecution, clan-based persecution, and other forms of non-state and structural persecution. Under the expansive reading, they are entitled to have their fear assessed substantively and to non-refoulement protection. Their situation is that they cannot safely remain in their origin country and have no alternative safe destination without international protection. If the expansive reading collapses, they revert to the restrictive reading's victim set (much smaller) or procedural reading's outcome uncertainty. Their exit from the constraint is death or return to persecution.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_claimants_broad_cohort, beneficiary,
    powerless, biographical, trapped, global).

% UNHCR, International Refugee Law Association, regional human rights bodies (ECtHR, IACtHR), Amnesty International, Human Rights Watch, and other NGOs that defend and propagate the expansive reading in litigation, advisory opinions, and advocacy. They benefit from the reading being operationalized (their mandate is vindicated, their litigation strategy succeeds, their interpretive authority is recognized). They partly set the agenda by publishing guidance documents, filing amicus briefs, and shaping state practice through training and advocacy. They could exit by repudiating the expansive reading and adopting a more restrictive position, but such a move would undermine their organizational mission and legitimacy.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, human_rights_advocacy_organizations, beneficiary,
    organized, generational, mobile, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, human_rights_advocacy_organizations, agenda_setter).

% States (Canada, many European states, Latin American states) that have adopted the expansive humanitarian reading in their domestic law, case law, and administrative practice. They set the agenda by implementing substantive assessment procedures, recognizing broad particular social groups, and rejecting interdiction and offshore processing as refoulement violations. They bear enforcement costs (expensive asylum systems, inability to exclude claimants via categorical rejection, reputational costs of deportations). They could exit by denouncing the Convention or adopting the restrictive reading, but both moves are politically costly (human rights criticism, reputational damage in international forums, inability to claim moral leadership on human rights). Their exit options are constrained but not trapped.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, signatory_states_broad_interpretation_coalition, agenda_setter,
    institutional, generational, constrained, global).

% States (Australia, US under restrictive administrations, Hungary, Denmark under certain governments) that interpret the Convention narrowly, using categorical exclusions, interdiction, offshore processing, and third-country agreements to minimize asylum responsibility. They pay the cost of the expansive reading's legitimacy: it delegitimizes their restrictive interpretation in human rights forums, creates litigation risk when NGOs bring cases challenging their practices, and subjects them to international scrutiny and criticism. They are partly excluded (the reading's humanitarian mandate does not recognize their sovereignty-respecting interpretation as legitimate; they are not at the table where the humanitarian interpretation is set). They could exit by denouncing the Convention, but this is politically costly. More realistically, they navigate by partially complying (formal adherence to the Convention + restrictive interpretation) and arguing about how the text should be read.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, signatory_states_restrictive_interpretation_coalition, payer,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, signatory_states_restrictive_interpretation_coalition, excluded).

% Governments and armed groups in countries where persecution originates (gang violence, gender-based violence, state persecution, clan-based violence, generalized conflict). They are excluded from the refugee protection system but structurally defined by it: the expansive reading expands what counts as persecution, which defines them as persecutors and creates asylum outflows. They have no formal seat at the table where the Convention is interpreted; they cannot directly influence the reading's scope. They are trapped because they cannot exit the system — they are defined by the persecution mechanisms the reading classifies, and denying the harm would be implausible.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, origin_states_and_non_state_actors, excluded,
    institutional, generational, trapped, global).

% Immigration judges, asylum appeal boards, border officials, and administrative decision-makers who apply the expansive reading in individual cases. They are partly agenda-setters (their decisions operationalize the reading in concrete cases, create jurisprudence that shapes how the reading evolves) and partly constrained (they must follow the legal framework their state has adopted; if their state adopts the expansive reading, they are bound by it). They bear the cost of substantive assessment (time, resources, training, expertise required for fair adjudication). They could exit by adopting a different interpretation or by systematic non-compliance, but professional norms, legal obligation, and oversight mechanisms constrain these moves.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, domestic_adjudicators_and_immigration_officials, agenda_setter,
    institutional, biographical, constrained, national).

% Academic international law, UNHCR legal analysis, and the interpretive community that documents and analyzes how the Convention is read. They neither benefit from nor pay the constraint's costs; they observe and analyze. They have no formal enforcement role but provide legitimacy and interpretive authority through scholarship and guidance documents.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, analytical_observer_community, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, diffuse).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establish a binding international commitment that individuals fleeing persecution are entitled to cross-border protection; create a shared responsibility mechanism (asylum states agree to receive and assess claims; origin states agree to allow departure) and a unified definition of persecution so that protection is not arbitrary. Solve the problem of persecution victims with no safe alternative by creating a universal obligation of asylum assessment and non-refoulement.
% TRANSFER_FUNCTION: No material transfer in the structural sense. The constraint moves protective rights FROM the international system TO asylum claimants (the right to seek asylum, substantive assessment, non-refoulement protection). Administrative burdens move FROM origin states and asylum states TO asylum states (the cost of assessment and protection systems). Moral and political legitimacy moves FROM signatory states that implement the expansive reading (they gain human rights credibility) TO origin states and restrictive-interpretation states (they bear the reputational cost of denying asylum or narrowly interpreting persecution).
% ABSENT_VOICES: Origin states and non-state actors (persecutors, oppressive governments) would object that the expansive reading enlarges the definition of persecution, creates asylum outflows, and subjects them to criticism as persecutors — but they are excluded by design (the Convention is drafted by refugee-generating and refugee-receiving states, not by persecutors). Restrictive-interpretation states that prefer less expansive definitions of persecution would object that the reading constrains their sovereignty — they are in the conversation but their interpretation is delegitimized by the human rights framing. Asylum claimants who fall outside the broad definition (economic migrants, internally displaced persons not crossing borders) are also excluded — the Convention protects cross-border persecution victims, not all displaced persons.
% DISAPPEARANCE_RATIONALE: If this reading disappeared overnight (Convention collapsed, or restrictive interpretation became universal), millions of people fleeing persecution would lose their only international protection mechanism. The world would rearrange: many asylum claimants would be returned to persecution; asylum systems in signatory states would dissolve or narrow dramatically; the burden would shift to origin states (forced to manage persecution internally or allow emigration); humanitarian organizations would lose their primary legal framework; international human rights law would have lost one of its central protections. The reading's disappearance would be structurally catastrophic for asylum claimants and transformative for international humanitarian law.
% FOUNDING_PROBLEM: The Shoah and the post-1945 displacement crisis: persecution on a scale requiring international responsibility-sharing. The founders recognized that individual states could not manage persecution flows alone, and that universal human dignity required a binding commitment to protect people fleeing persecution across borders. The founding problem was: how do we ensure that people fleeing persecution have a safe destination, and that no state is overwhelmed by single-handedly absorbing displaced populations?
% FOUNDING_PROBLEM_CORROBORATION: Persecution continues globally: gender-based violence affecting women across countries with weak rule of law; LGBTQ+ persecution in dozens of states; clan-based violence in fragmented societies; generalized violence in conflict zones. UNHCR documentation shows 100+ million displaced persons globally as of 2024, with roughly 32 million refugees. Independent human rights organizations (Amnesty, Human Rights Watch), regional courts (ECtHR, IACtHR), and UNHCR all attest that contemporary persecution matches the scale and character that motivated the founding. The founding problem is not solved; it is ongoing. The expansive humanitarian reading claims that the Convention's mandate remains operative for this contemporary persecution and that narrow interpretations that exclude generalized violence, non-state persecution, or gender-based persecution are inconsistent with the founding commitment to universal protection.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.28, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(refugee_convention_text__expansive_humanitarian_reading_tests).
:- end_tests(refugee_convention_text__expansive_humanitarian_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.28 at interval end) because this reading does NOT extract from anyone — it EXPANDS who gets protected rather than leveraging protection to benefit some at the cost of others. The constraint's core function is genuine coordination: create a binding international commitment that persecution victims are protected across borders, that states cannot return people to danger, and that assessment procedures respect their dignity and agency. Beneficiaries are the asylum claimants whose persecution would be recognized under broad definitions; the advocacy organizations benefit instrumentally (their mandate is vindicated). Victims are... structurally minimal under this reading. The suppression (0.22) reflects the real enforcement cost: states must maintain border systems that allow asylum claims to be filed (not interdicted), must provide substantive hearings (not categorical rejections), must not externalize responsibility. This IS enforcement overhead, but it enforces protection, not extraction. Theater ratio (0.12) is low: the constraint's real function (humanitarian protection) and its performed function (humanitarian protection) are aligned; there is little performative activity masking extraction. The measurement series shows gentle rise over the interval (T=0 to T=30, roughly 1990s to 2020s): extractiveness rises slightly as the reading's scope widens and states feel constrained; suppression rises as enforcement machinery intensifies in response to larger asylum flows. But the absolute levels stay low because the reading's structural position is genuinely protective, not extractive. The claim/metric independence rule: I claim this is a Rope (genuine coordination on humanitarian protection) and the metrics describe low extraction, low suppression, low theater — the metrics support the claim, which is appropriate when the constraint's actual function matches its espoused function.
 *
 * PERSPECTIVAL GAP:
 *   The payer seat and the beneficiary seat should compute to different types, not because of hidden extraction but because of institutional position. From an asylum claimant's seat, the reading is a floor of protection they depend on — if it disappears, they face return to persecution. From a signatory state's seat (e.g., Denmark under a restrictive government), the reading is an obligation that constrains sovereignty and requires expensive border procedures. Both parties are coordinating (the state accepted the Convention; the claimant relies on its implementation), but they experience the constraint's cost asymmetry differently: the claimant bears the cost of narrowing (false negatives, returned persecution); the state bears the cost of broadening (expensive assessment, inability to exclude). Yet NEITHER seat bears extraction in the structural sense — there is no capturer of the gains, no leveraged benefit, no victim-to-beneficiary transfer. This is why the constraint is correctly characterized as Rope despite the cost asymmetry. The engine computes directionality from the structural data (beneficiary/victim declarations, exit options, power levels); I have declared no victims and multiple beneficiaries, which correctly expresses that this reading's operation is protective rather than extractive.
 *
 * DIRECTIONALITY LOGIC:
 *   Asylum claimants are beneficiaries (the reading expands who gets protected; d low, near beneficiary end, their effective extraction is negative — they are subsidized by the constraint). Human rights organizations are beneficiaries (their mandate is vindicated; d low). Signatory states carry complex directionality: they are partly agenda-setters (they chose to adopt the Convention and can interpret it), partly constrained (the reading binds them), partly powerful (they control borders and adjudication). The reading's existence does NOT extract from states in the structural sense; it coordinates their action around a shared humanitarian commitment. If a state wanted to exit, it could denounce the Convention (high cost, reputational damage, but not impossible). This is constrained exit, not trapped exit — d somewhere in the middle, d=0.45 or so. The origin states and non-state actors are excluded (they are not parties to the Convention; they would oppose the reading because it creates asylum outflows). Analytical observers have d=0.5 (symmetric, neither benefiting nor paying). The absence of declared victims reflects the reading's genuine coordination structure: under this reading, there is no structural class that bears extraction costs FROM the constraint itself. States bear administrative costs, but those are enforcement overhead, not extraction. The reading's opponents (restrictive sovereigntists) claim it extracts from states by forcing them to internalize responsibility; that is a different claim, instantiated in a different constraint story (the restrictive sovereignty reading), which WOULD declare states as victims.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem is the Shoah and the displacement crisis of 1945–1951: persecution on a scale requiring international responsibility-sharing. The expansive humanitarian reading claims that problem is LIVE and that the Convention's mandate remains operative for contemporary persecution (gender-based violence, gang violence, LGBTQ+ persecution, clan-based persecution). The restrictive sovereignty reading claims the problem is DEAD (modern persecution is not genocide-scale; stable states manage persecution through domestic law) and the Convention persists as bureaucratic inertia. The procedural integrity reading claims the problem's status is CONTESTED (humanitarian need is real, but the Convention's primary value is procedural fairness, not humanitarian outcome). Under this reading, mandatrophy has NOT occurred: the founding mandate (protect people fleeing persecution on a worldwide scale) aligns with the contemporary operation (broad assessment of persecution, non-refoulement enforcement, protection across borders). The constraint solves the problem it was built to solve, not because the problem is unsolved but because the problem is ongoing (persecution continues; asylum flows continue; the Convention's protection remains the only international mechanism for managing cross-border persecution). This is a living, functional constraint under this reading.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    humanitarian_vs_sovereignty_framing,
    'Is the Refugee Convention''s core mandate humanitarian protection of individuals fleeing generalized violence and social persecution, or a bounded sovereignty-respecting floor permitting broad state discretion in interpretation?',
    'Textual exegesis of the Convention''s preamble and operative articles across multiple authoritative commentaries; examination of state practice at accession and early implementation; interpretation guidance from the UNHCR Executive Committee and ICJ advisory opinions.',
    'If humanitarian protection is the core mandate, ''well-founded fear'' and ''particular social group'' widen to include generalized violence and gender/LGBTQ+/clan persecution; if sovereignty-respecting floor is core, these categories narrow to individualized, state-centric persecution. Terminal classification spans Rope (coordination on broad protection) to Snare (leveraging sovereignty to exclude vulnerable cohorts). The reading determines which.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_vs_sovereignty_framing, conceptual, 'Whether the Convention''s primary purpose is expansive humanitarian protection or bounded sovereignty-preserving floor.').

omega_variable(
    generalized_violence_epistemology,
    'Does ''well-founded fear of persecution'' encompass generalized violence from which vulnerable populations cannot escape (gang violence, communal warfare, gender-based violence affecting entire populations), or only targeted persecution directed at the individual claimant by agents with persecution-specific animus?',
    'Empirical documentation of persecution mechanisms in origin countries; comparative asylum adjudication patterns across jurisdictions; testimony from LGBTQ+ individuals, women, and clan-based persecution survivors regarding whether violence is individualized or structural.',
    'If generalized violence is encompassed, asylum seeker cohorts expand by orders of magnitude; if individualization is required, vast populations fleeing objectively life-threatening conditions are excluded. Extracted population (those in genuine danger excluded by narrow reading) could exceed 10 million globally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(generalized_violence_epistemology, empirical, 'Whether persecution must be targeted-individualized or can be structural/generalized.').

omega_variable(
    refoulement_doctrine_scope,
    'Does the non-refoulement principle prohibit indirect exclusion (interdiction, offshore processing, third-country agreements that shift liability) as well as direct return, or only direct return?',
    'Textual reading of Article 33 (''shall not expel or return''); case law from IACtHR, ECtHR, and other regional bodies; state practice on interdiction and externalized processing; UNHCR position statements.',
    'If indirect exclusion violates non-refoulement, entire offshore processing regimes (Australia, Denmark externalized processing, EU externalization agreements) become illegal under this reading. If only direct return is prohibited, externalization is permitted. The measure of the constraint''s extractive cost depends entirely on this boundary.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(refoulement_doctrine_scope, conceptual, 'Whether non-refoulement encompasses indirect exclusion or only direct return.').

omega_variable(
    particular_social_group_linguistic_stability,
    'Is ''particular social group'' a term of art with relatively stable referents (gender, LGBTQ+ identity, clan affiliation, caste), or an open category that adapts to persecution mechanisms as they evolve?',
    'Textual exegesis of the 1951 Convention and 1967 Protocol; examination of UNHCR Handbook and subsequent guidance; comparative jurisprudence from UNHCR''s Refugee Status Determination procedures across regions; analysis of whether social groups listed in early case law have remained stable.',
    'Stable-referent reading widens over time as categories are formally recognized (gender, LGBTQ+ status are now broadly accepted; persecution on account of caste or indigenous identity status are contested but gaining recognition). Open-category reading creates immediate protection but also institutional uncertainty — states cannot predict what counts as a ''particular social group'' in future adjudication.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(particular_social_group_linguistic_stability, conceptual, 'Whether ''particular social group'' has stable referents or is an open adaptive category.').

omega_variable(
    duty_to_assess_all_claims_constructively,
    'Does broad protection require proactive substantive assessment of all claims (assuming credibility, applying favorable inferences, assigning burden of proof to the state to disprove fear), or do states retain discretion to construct or reject claim categories wholesale (e.g., by administrative categorization of what counts as persecution)?',
    'Review of asylum adjudication procedures in jurisdictions adopting broad vs. restrictive readings; examination of burden-of-proof allocation; documentation of how many claims survive substantive assessment vs. categorical rejection.',
    'Constructive assessment would require individualized hearings, appeal rights, and favorable burden allocation — expensive and time-consuming, expanding protection dramatically. Categorical rejection permits administrative efficiency at the cost of false negatives (rejected claims of genuine danger). This determines whether ''broad protection'' is institutional-resource-intensive or merely nominal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duty_to_assess_all_claims_constructively, empirical, 'Whether broad protection institutionally requires proactive substantive assessment of all claims.').

omega_variable(
    reading_kernel_coherence,
    'Is this expansive humanitarian reading internally coherent as a single interpretation of the Convention''s text, or does it require reading different articles (Article 1A definition, Article 33 non-refoulement, particular social group cases) according to different hermeneutical canons (broad text, narrow procedure, etc.)?',
    'Unified exegetical analysis applying one hermeneutical canon (humanitarian purpose maximization, or purposive interpretation, or teleological reading) across all operative articles and seeing whether the reading remains stable.',
    'If coherent under one canon, the reading is a single structural claim about the Convention''s unified mandate. If it requires mixed canons, it is a constructed compromise between humanitarian and sovereignty-protecting readings — neither pure humanitarian nor pure sovereignty-respecting. The terminal classification (Rope vs. Snare) depends on this coherence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(reading_kernel_coherence, conceptual, 'Whether the expansive reading is internally coherent under a unified hermeneutical principle.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refu_tr_t0, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(refu_tr_t10, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 10, 0.1).
narrative_ontology:measurement(refu_tr_t20, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 20, 0.12).
narrative_ontology:measurement(refu_tr_t30, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(refu_be_t0, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(refu_be_t10, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 10, 0.22).
narrative_ontology:measurement(refu_be_t20, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(refu_be_t30, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 30, 0.28).

% Suppression requirement over time
narrative_ontology:measurement(refu_su_t0, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0, 0.18).
narrative_ontology:measurement(refu_su_t10, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 10, 0.2).
narrative_ontology:measurement(refu_su_t20, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 20, 0.22).
narrative_ontology:measurement(refu_su_t30, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 30, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(refugee_convention_text__expansive_humanitarian_reading, 0.18).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% The refugee_convention_text kernel decomposes into three distinct constraint stories, each instantiating a coherent reading of the 1951 Convention and 1967 Protocol. All three read the same fixed text but apply different hermeneutical canons (humanitarian-maximizing vs. sovereignty-respecting vs. procedure-focused), generating different victim sets, different enforcement costs, and different terminal classifications. The expansive_humanitarian_reading (this story) treats persecution broadly, non-state actors as potential persecutors, and particular social groups to include gender/LGBTQ+/clan persecution. The restrictive_sovereignty_reading treats persecution narrowly, requires state animus and individualized targeting, and interprets particular social groups restrictively. The procedural_integrity_reading focuses on the fairness of adjudication regardless of outcome breadth. All three stories must be linked via network.affects_constraints to signal their kinship in the same kernel; the trilogy cannot be understood independently.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
