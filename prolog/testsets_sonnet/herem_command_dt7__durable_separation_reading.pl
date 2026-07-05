% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_herem_command_dt7__durable_separation_reading, []).

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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem as Timeless Mandate for Categorical Separation (Deuteronomy 7 durable-application reading)
 *   domain: religious/ethical/legal
 *
 * SUMMARY:
 *   Deuteronomy 7 commands Israel to show no mercy to seven named Canaanite
 *   nations, to destroy their religious infrastructure, and to refuse
 *   intermarriage on pain of covenant corruption. The durable-separation
 *   reading treats this command's underlying logic — categorical boundary
 *   maintenance, contamination anxiety, and divine-command override of
 *   relational ethics — as a standing, transferable mandate rather than a
 *   settlement-era artifact. This reading has recurrently supplied
 *   theological cover for endogamy enforcement, exclusionary communal policy,
 *   and, in its most severe historical invocations, violence against groups
 *   analogized to the 'seven nations.' The claimed type (tangled_rope) and
 *   the authored metrics are independent facts: the reading does perform a
 *   real coordination function (minority identity preservation) while also
 *   authorizing substantial, actively-enforced extraction from those it
 *   categorizes as outside or contaminating — that combination is exactly the
 *   tangled_rope signature, and the engine's per-seat computation is expected
 *   to diverge sharply between boundary-maintainer and designated-outsider
 *   seats.
 *
 * KEY AGENTS:
 *   - covenant_community_boundary_maintainers: agenda_setter/beneficiary (institutional/arbitrage) — administers and profits from the boundary
 *   - intermarriage_seeking_covenant_members: payer (moderate/constrained) — bears direct extraction on marital autonomy
 *   - designated_outsider_populations: excluded (powerless/trapped) — categorized without standing to contest it
 *   - descendants_of_mixed_unions: payer (powerless/trapped) — inherits contested status by lineage
 *   - converts_of_contested_status: payer (powerless/constrained) — bears heightened scrutiny cost
 *   - ethnonationalist_religious_movements: beneficiary (organized/mobile) — extracts political and mobilization capital
 *   - prophetic_universalist_critics: excluded (moderate/constrained) — marginalized from interpretive authority despite internal tradition standing
 *   - biblical_scholars_comparative_historians: observer (analytical/analytical) — documents reception history without institutional stake
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.71).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.79).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.71).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem as Timeless Mandate for Categorical Separation (Deuteronomy 7 durable-application reading)").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "religious/ethical/legal").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '7067309a-635f-482f-8085-d3ee1b38213e').
narrative_ontology:cs_kernel_codification('7067309a-635f-482f-8085-d3ee1b38213e', fixed_text).
narrative_ontology:cs_authority_grounding('7067309a-635f-482f-8085-d3ee1b38213e', lineage).
narrative_ontology:cs_interpretation_layer_present('7067309a-635f-482f-8085-d3ee1b38213e').
narrative_ontology:cs_reading_relation('7067309a-635f-482f-8085-d3ee1b38213e', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('7067309a-635f-482f-8085-d3ee1b38213e', herem_command_dt7__allegorical_displacement_reading, coexists_with).
narrative_ontology:cs_axiom('7067309a-635f-482f-8085-d3ee1b38213e', foundational, divine_mandate_is_perpetually_binding_not_historically_bounded).
narrative_ontology:cs_axiom_status(divine_mandate_is_perpetually_binding_not_historically_bounded, holdable).
narrative_ontology:cs_axiom_grounding('7067309a-635f-482f-8085-d3ee1b38213e', divine_mandate_is_perpetually_binding_not_historically_bounded, deontological).
narrative_ontology:cs_axiom('7067309a-635f-482f-8085-d3ee1b38213e', foundational, categorical_separation_from_designated_outsiders_is_literal_not_typological).
narrative_ontology:cs_axiom_status(categorical_separation_from_designated_outsiders_is_literal_not_typological, holdable).
narrative_ontology:cs_axiom_grounding('7067309a-635f-482f-8085-d3ee1b38213e', categorical_separation_from_designated_outsiders_is_literal_not_typological, conventional).
narrative_ontology:cs_reference_frame('7067309a-635f-482f-8085-d3ee1b38213e', settlement_era_covenant_boundary_mandate).
narrative_ontology:cs_drift_state('7067309a-635f-482f-8085-d3ee1b38213e', contemporary_pluralist_era, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('7067309a-635f-482f-8085-d3ee1b38213e', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_community_boundary_maintainers).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, endogamy_enforcing_clergy).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, ethnonationalist_religious_movements).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_seeking_covenant_members).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, designated_outsider_populations).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, descendants_of_mixed_unions).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, converts_of_contested_status).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Clergy, communal authorities, and legal bodies who interpret herem as a standing mandate for identity preservation. They administer membership boundaries, adjudicate marriage eligibility, and invoke the divine-command frame to justify categorical separation. They set the terms under which belonging is granted or withdrawn, and their institutional authority is itself constituted by policing this boundary.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_community_boundary_maintainers, agenda_setter,
    institutional, civilizational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_community_boundary_maintainers, beneficiary).

% Individuals within the covenant community who wish to marry outside the designated boundary. Under this reading their autonomy is directly restricted; the herem logic reframes their personal choice as a contamination risk to communal identity. Exit means either compliance, quiet defection with loss of communal standing, or protracted communal conflict — leaving the covenant community's institutional protections and social fabric entirely.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_seeking_covenant_members, payer,
    moderate, biographical, constrained, local).

% Groups categorized as the modern or perennial analogue of the seven nations — whoever the interpreting authority designates as the contamination threat. Under the durable-separation reading, they have no standing in the covenant conversation at all; the text's application to them is decided entirely by the boundary-maintaining authority, and historically this reading has legitimated exclusion, expulsion, or violence against groups so designated.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, designated_outsider_populations, excluded,
    powerless, generational, trapped, regional).

% Children and grandchildren of intermarriages, whose covenant status becomes contested retroactively under a durable-separation frame. They inherit the disputed boundary without having chosen either side of it; their exit options are essentially nonexistent since the classification is imposed on them by lineage, not by their own action.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, descendants_of_mixed_unions, payer,
    powerless, generational, trapped, local).

% Individuals who seek to join the covenant community from outside it. The durable-separation reading treats conversion as always suspect relative to birthright membership, subjecting converts to heightened scrutiny, delayed acceptance, or permanent second-tier status — bearing the cost of a boundary they are trying to cross legitimately.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, converts_of_contested_status, payer,
    powerless, biographical, constrained, local).

% Political-religious movements that invoke the durable-separation reading to legitimate land claims, exclusionary citizenship policy, or violence against designated outsider groups, citing divine command as override to ordinary ethical constraint. They gain political mobilization capital and moral cover from treating the mandate as timeless and literal.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, ethnonationalist_religious_movements, beneficiary,
    organized, generational, mobile, national).

% Voices within the same tradition (drawing on Ruth, Jonah, Isaiah's nations-streaming-to-Zion texts, or later covenant theology) who argue the durable-separation reading misreads the canon's own trajectory. They are structurally excluded from adjudicating authority in communities where boundary-maintainers control interpretive institutions, even though their reading originates from within the same textual tradition.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, prophetic_universalist_critics, excluded,
    moderate, generational, constrained, national).

% Historical-critical and comparative-religion scholars who examine herem's ancient Near Eastern conquest-ideology parallels, its literary function, and its reception history. They document how the durable-separation reading has been mobilized across centuries without holding institutional stakes in any single community's boundary enforcement.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, biblical_scholars_comparative_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, covenant_community_boundary_maintainers).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Genuinely solves a real problem for a small, vulnerable ethno-religious community: maintaining a distinct identity, ritual practice, and internal solidarity against absorption by larger surrounding cultures. Bounded membership criteria let a minority tradition persist across generations rather than dissolving through unregulated intermixture.
% TRANSFER_FUNCTION: Moves marital autonomy, inheritance standing, and social belonging away from individuals whose choices or lineage cross the designated boundary, and toward the boundary-maintaining authority's continued institutional relevance and toward in-group members who benefit from clarified, defended membership status. It also moves physical security and land claims away from designated outsider populations when the reading is invoked politically.
% ABSENT_VOICES: Designated outsider populations have no voice in how they are categorized — the text was written about them, not by them, and the durable reading gives them no standing to contest the classification. Descendants of mixed unions and contested converts are litigated over, not consulted. Prophetic-universalist voices within the same tradition are marginalized wherever boundary-maintaining institutions control interpretive authority.
% DISAPPEARANCE_RATIONALE: Boundary-maintaining authorities and ethnonationalist movements would say the community's identity unravels without the mandate — assimilation, intermarriage, and dissolution of distinct practice would follow rapidly. Intermarriage-seeking members, contested converts, and designated outsiders would say the world simply becomes less coercive and more just, with no actual loss of coordinable religious practice, since attachment_coordination and shared ritual life do not require categorical exclusion to function. The dispute is genuinely unresolved because it turns on a contested empirical claim (does communal identity require categorical exclusion to persist) that different parties answer from irreconcilable premises.
% FOUNDING_PROBLEM: In its Deuteronomic setting, herem addressed a specific settlement-era anxiety: a small covenant people surrounded by populations whose religious practices (child sacrifice, fertility cults tied to political alliance) were read as existentially assimilative threats to a fragile, newly-formed national-religious identity.
% FOUNDING_PROBLEM_CORROBORATION: Boundary-maintaining authorities and ethnonationalist movements attest the problem is still live — identity dissolution through intermarriage and assimilation is treated as a present, not historical, danger. Historical-critical scholars, comparative religion historians, and prophetic-universalist theologians from within the same tradition attest the founding problem was time- and context-bound to an ancient geopolitical situation with no contemporary structural analogue, and that its durable application is a reading choice, not an inherited necessity — this corroboration comes from outside the beneficiary set (academic and internal dissenting-theological seats, not the boundary-maintaining institutions themselves).
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, contested).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-05',
    'no_scope_rebuild_sonnet', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.71, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(herem_command_dt7__durable_separation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(herem_command_dt7__durable_separation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.71) is substantial but not maximal: the reading does perform genuine identity-preservation coordination for a minority tradition, so the extraction is layered onto real coordination rather than being pure predation — hence tangled_rope, not snare. Suppression (0.79) is high because durable application requires active, ongoing enforcement (marriage vetting, boundary policing, sometimes political violence) rather than passive acceptance. Theater ratio is comparatively low (0.28) because the enforcement mechanisms described (marriage courts, communal exclusion, political mobilization) are functionally real, not merely performative — though it rises modestly over the measured interval as boundary maintenance becomes more institutionally routinized and less tied to acute existential threat. Accessibility collapse (0.62) reflects that once a community accepts the durable-separation framing as literally binding, internal alternatives (reinterpretation, appeal to prophetic-universalist strands) become hard to raise without appearing to reject scriptural authority itself — though the collapse is not total, since dissenting readings persist within the same tradition. Resistance (0.58) is real: contested converts, mixed-union descendants, and prophetic-universalist critics actively push back, but from structurally weaker positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Boundary-maintaining authorities and ethnonationalist movements sit near the beneficiary pole: they gain institutional relevance, mobilization capital, and moral cover from treating the mandate as timeless and literal, with mobile-to-arbitrage exit options protecting them from bearing the costs their interpretation imposes on others. Intermarriage-seeking members, contested converts, and descendants of mixed unions sit near the target pole: the reading directly restricts their autonomy or imposes disputed status on them by inheritance, with constrained-to-trapped exit. Designated outsider populations sit at the extreme target end — they are categorized by a text and a reading tradition in which they have zero interpretive standing, and in the reading's most severe historical applications this categorization has legitimated violence against them.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents two mislabeling errors symmetrically: it prevents dismissing the reading as pure invented extraction (there is a real, historically documented coordination function — minority identity survival against assimilative pressure — that a snare label would erase), and it prevents laundering the extraction as pure benign coordination (a rope label would erase the documented cost borne by intermarriage-seeking members, contested converts, and designated outsiders, and would erase the historical violence-legitimation function). The founding_problem/disappearance_verdict mismatch check is central here: founding_problem_status is authored as contested rather than dead, because unlike a clean mandatrophy case, boundary-maintainers can correctly point to some communities where assimilative dissolution is a live risk — the corpus should not force a false zombie-flag onto every invocation of this reading, only onto invocations where corroboration from outside the beneficiary set establishes the founding problem has in fact lapsed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    durable_vs_bounded_reading_choice,
    'Is the durable-separation reading a defensible extension of Deuteronomy 7''s own internal logic, or does it require suppressing the text''s explicit settlement-period markers (named nations, named land, named historical moment) to sustain perpetual application?',
    'Close comparison of the durable-separation reading''s interpretive moves against the contextual_supersession_reading and allegorical_displacement_reading siblings — specifically whether the durable reading can account for the text''s geographically and historically specific referents without treating them as incidental.',
    'If the settlement-period markers are load-bearing rather than incidental, the durable-separation reading''s claim to timelessness is structurally weaker than its rival readings, which would shift classification pressure toward reading it as an extractive overreach riding on a coordination function that the text itself did not intend to generalize.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(durable_vs_bounded_reading_choice, conceptual, 'Whether the perpetual-application move is textually licensed or an interpretive imposition.').

omega_variable(
    coordination_extraction_separability_herem,
    'Can the genuine identity-preservation coordination function (a real, minority-survival problem) be achieved through non-categorical, non-exclusionary means, or does the durable-separation reading correctly identify categorical exclusion as structurally necessary to that function?',
    'Comparative study of minority religious communities that maintain distinct identity across generations WITHOUT categorical intermarriage prohibition or outsider-contamination framing (e.g., communities relying on voluntary affiliation, ritual distinctiveness, or educational transmission rather than boundary policing) versus those that do use categorical exclusion, controlling for community size and surrounding assimilative pressure.',
    'If identity preservation is achievable without categorical exclusion, the extraction measured here is separable from the coordination function and this reading''s specific mechanism is optional extraction riding on a real problem; if categorical exclusion is empirically necessary at small community scale, part of the measured extraction is closer to an inherent coordination cost.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_extraction_separability_herem, empirical, 'Whether categorical exclusion is necessary to the coordination function or a separable extractive add-on.').

omega_variable(
    designated_outsider_referent_indeterminacy,
    'Under the durable-separation reading, who counts as the contemporary referent of ''the seven nations'' — and who has authority to make that designation?',
    'Trace how the durable-separation reading has been applied across distinct historical and political contexts to identify whether the referent-designation power is itself unconstrained (i.e., whoever holds interpretive authority can nominate any group as the contamination threat) or bounded by some independent criterion.',
    'If referent-designation is unconstrained, the victim set (designated_outsider_populations) is open-ended by construction — any group can be nominated by whoever holds boundary-maintaining authority, which substantially raises the severity of the suppression and violence-legitimation findings beyond what a fixed, bounded victim set would imply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(designated_outsider_referent_indeterminacy, conceptual, 'Whether the reading''s outsider category is a fixed historical set or an open-ended designation slot.').

omega_variable(
    kernel_framing_alternative_check,
    'Is the obvious framing here (a scriptural command interpreted by religious authorities) the only defensible one, or does a legitimacy-claim layer above it — the assertion that divine command categorically overrides ordinary relational ethics — constitute a second, distinct kernel element that this story has folded into the base reading rather than treating separately?',
    'Compare classification outcomes if the divine-command-override-of-ethics premise were treated as its own separable axiom subject to independent contest, versus folded into the durable-separation reading as done here.',
    'If treated separately, the divine-command-override premise might itself be contested independently across all three sibling readings (i.e., even the allegorical or supersession readings might retain some form of the override premise for other commands), which would mean this story''s foundational axiom is doing double duty as both the separation claim and the override claim — a decomposition risk flagged here rather than resolved.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_framing_alternative_check, conceptual, 'Whether the divine-command-override element deserves its own kernel treatment separate from the separation claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(here_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(here_tr_t0, observed).
narrative_ontology:measurement(here_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.14).
narrative_ontology:measurement_basis(here_tr_t20, observed).
narrative_ontology:measurement(here_tr_t40, herem_command_dt7__durable_separation_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement_basis(here_tr_t40, observed).
narrative_ontology:measurement(here_tr_t60, herem_command_dt7__durable_separation_reading, theater_ratio, 60, 0.21).
narrative_ontology:measurement_basis(here_tr_t60, observed).
narrative_ontology:measurement(here_tr_t80, herem_command_dt7__durable_separation_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement_basis(here_tr_t80, observed).
narrative_ontology:measurement(here_tr_t100, herem_command_dt7__durable_separation_reading, theater_ratio, 100, 0.28).
narrative_ontology:measurement_basis(here_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(here_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement_basis(here_be_t0, observed).
narrative_ontology:measurement(here_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.6).
narrative_ontology:measurement_basis(here_be_t20, observed).
narrative_ontology:measurement(here_be_t40, herem_command_dt7__durable_separation_reading, base_extractiveness, 40, 0.65).
narrative_ontology:measurement_basis(here_be_t40, observed).
narrative_ontology:measurement(here_be_t60, herem_command_dt7__durable_separation_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement_basis(here_be_t60, observed).
narrative_ontology:measurement(here_be_t80, herem_command_dt7__durable_separation_reading, base_extractiveness, 80, 0.7).
narrative_ontology:measurement_basis(here_be_t80, observed).
narrative_ontology:measurement(here_be_t100, herem_command_dt7__durable_separation_reading, base_extractiveness, 100, 0.71).
narrative_ontology:measurement_basis(here_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(here_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.62).
narrative_ontology:measurement_basis(here_su_t0, observed).
narrative_ontology:measurement(here_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.66).
narrative_ontology:measurement_basis(here_su_t20, observed).
narrative_ontology:measurement(here_su_t40, herem_command_dt7__durable_separation_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement_basis(here_su_t40, observed).
narrative_ontology:measurement(here_su_t60, herem_command_dt7__durable_separation_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement_basis(here_su_t60, observed).
narrative_ontology:measurement(here_su_t80, herem_command_dt7__durable_separation_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement_basis(here_su_t80, observed).
narrative_ontology:measurement(here_su_t100, herem_command_dt7__durable_separation_reading, suppression_requirement, 100, 0.79).
narrative_ontology:measurement_basis(here_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% This story is one of three siblings decomposing the natural-language label 'the herem command of Deuteronomy 7' per the ε-invariance principle: durable_separation_reading (this file, tangled_rope, high extraction on intermarriage autonomy and expansive victim set), contextual_supersession_reading (historically-bounded, morally superseded — expected lower extraction, narrower or absent contemporary victim set), and allegorical_displacement_reading (typological/internal-moral reading — expected minimal extraction, no literal victim set since 'nations' denote spiritual states, not ethnic groups). Each carries its own ε, its own beneficiary/victim structure, and its own classification; they are linked here via affects_constraints because adopting one reading structurally changes the legitimacy conditions and resource availability for the others within a given interpretive community (a community's institutional commitment to this reading makes the sibling readings harder to sustain as live options, and vice versa).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
