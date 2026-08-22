% ============================================================================
% CONSTRAINT STORY: herem_command_dt7__durable_separation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   constraint_id: herem_command_dt7__durable_separation_reading
 *   human_readable: Herem Command (Deut 7) — Durable Separation Reading
 *   domain: biblical hermeneutics/religious ethics/commitment-system analysis
 *
 * SUMMARY:
 *   Under its durable-separation reading, the herem command of Deuteronomy 7
 *   operates as a perpetual divine mandate: the covenant community maintains
 *   a bounded membership, confines marriage inside the boundary, and holds
 *   designated outsiders in categorical separation — as contamination threats
 *   by origin, not conduct. This story instantiates THAT reading as one
 *   clean, epsilon-invariant constraint: the arrangement under assessment is
 *   the herem separation regime as this reading extends it (all non-covenant
 *   outsiders categorically separated, intermarriage autonomy surrendered,
 *   violence-bearing clauses retained as divine-command warrants). The
 *   sibling readings of the same kernel are separate constraint files linked
 *   through the network block; their structural deltas are documented there
 *   and in the omega variables, not averaged into this one. KEY AGENTS (by
 *   structural relationship): - covenant_boundary_administrators:
 *   Agenda-setter (institutional/identity_locked) — administers the boundary;
 *   the institution is constituted by the mandate it enforces. -
 *   covenant_endogamous_households: Primary beneficiary
 *   (organized/constrained) — receives identity continuity and mutual aid;
 *   pays a narrowed marriage field. - intermarriage_autonomy_seekers: Target
 *   (moderate/identity_locked) — bears the intermarriage-autonomy extraction
 *   from inside. - prospective_intermarried_spouses: Target and excluded
 *   voice (powerless/trapped) — the far side of the prohibited union, judged
 *   by category. - non_covenant_outsider_populations: Primary target
 *   (powerless/trapped) — categorical exclusion and violence legitimation by
 *   origin. - theological_dissenters_within_tradition: Target
 *   (moderate/identity_locked) — pays for contesting the mandate from within.
 *   - hermeneutic_scholars: Analytical observer (institutional/analytical) —
 *   maps the reading's operation and costs without enforcement authority.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(herem_command_dt7__durable_separation_reading, 0.82).
domain_priors:suppression_score(herem_command_dt7__durable_separation_reading, 0.78).
domain_priors:theater_ratio(herem_command_dt7__durable_separation_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, extractiveness, 0.82).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(herem_command_dt7__durable_separation_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(herem_command_dt7__durable_separation_reading, tangled_rope).
narrative_ontology:human_readable(herem_command_dt7__durable_separation_reading, "Herem Command (Deut 7) — Durable Separation Reading").
narrative_ontology:topic_domain(herem_command_dt7__durable_separation_reading, "biblical hermeneutics/religious ethics/commitment-system analysis").

domain_priors:requires_active_enforcement(herem_command_dt7__durable_separation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(herem_command_dt7__durable_separation_reading, '8b034595-67f9-4743-99f5-3784b112f6a2').
narrative_ontology:cs_kernel_codification('8b034595-67f9-4743-99f5-3784b112f6a2', fixed_text).
narrative_ontology:cs_authority_grounding('8b034595-67f9-4743-99f5-3784b112f6a2', lineage).
narrative_ontology:cs_interpretation_layer_present('8b034595-67f9-4743-99f5-3784b112f6a2').
narrative_ontology:cs_reading_relation('8b034595-67f9-4743-99f5-3784b112f6a2', herem_command_dt7__contextual_supersession_reading, forecloses).
narrative_ontology:cs_reading_relation('8b034595-67f9-4743-99f5-3784b112f6a2', herem_command_dt7__allegorical_displacement_reading, forecloses).
narrative_ontology:cs_axiom('8b034595-67f9-4743-99f5-3784b112f6a2', foundational, divine_mandate_timelessly_binding).
narrative_ontology:cs_axiom_status(divine_mandate_timelessly_binding, holdable).
narrative_ontology:cs_axiom_grounding('8b034595-67f9-4743-99f5-3784b112f6a2', divine_mandate_timelessly_binding, theological).
narrative_ontology:cs_axiom('8b034595-67f9-4743-99f5-3784b112f6a2', foundational, bounded_membership_divinely_obligatory).
narrative_ontology:cs_axiom_status(bounded_membership_divinely_obligatory, holdable).
narrative_ontology:cs_axiom_grounding('8b034595-67f9-4743-99f5-3784b112f6a2', bounded_membership_divinely_obligatory, deontological).
narrative_ontology:cs_reference_frame('8b034595-67f9-4743-99f5-3784b112f6a2', perpetual_covenant_boundary_order).
narrative_ontology:cs_drift_state('8b034595-67f9-4743-99f5-3784b112f6a2', contemporary_pluralist_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('8b034595-67f9-4743-99f5-3784b112f6a2', '').
narrative_ontology:cs_kernel_id(herem_command_dt7__durable_separation_reading, herem_command_dt7).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_boundary_administrators).
narrative_ontology:constraint_beneficiary(herem_command_dt7__durable_separation_reading, covenant_endogamous_households).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, non_covenant_outsider_populations).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, intermarriage_autonomy_seekers).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, prospective_intermarried_spouses).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, theological_dissenters_within_tradition).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(herem_command_dt7__durable_separation_reading, covenant_endogamous_households).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, divine_command_supremacy_doctrine).
narrative_ontology:constraint_vindicates(herem_command_dt7__durable_separation_reading, categorical_election_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret, administer, and enforce the boundary: adjudicate who counts as covenant member, police intermarriage, discipline boundary-crossers, and transmit the mandate to each generation as timeless. The institution's authority is constituted by the mandate it administers — abandoning it would dissolve the institution's own warrant. It collects adjudicatory jurisdiction over marriage and membership, communal deference, and the standing that accrues to gatekeepers.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_boundary_administrators, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Marry within the boundary, raise children inside it, and transmit patrimony, language, and practice down the generations. They receive continuity insurance: children who remain within the community, a legible inherited identity, dense mutual-aid networks, and assurance that the group will still exist for their grandchildren. They pay by accepting a narrowed marriage field for their children and by absorbing enforcement duties — withholding table fellowship from, or mourning as lost, kin who marry out.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, covenant_endogamous_households, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, covenant_endogamous_households, payer).

% Community members who fall in love across the boundary or wish to marry outside it. They face shunning, formal disowning, exclusion from communal ritual life, and in some communities loss of burial rights. Their attachment to the community is constitutive of who they are — leaving means losing family, belonging, and standing before God as they understand it; staying means renouncing the chosen partner. Either horn of the dilemma costs something irreplaceable.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, intermarriage_autonomy_seekers, payer,
    moderate, biographical, identity_locked, regional).

% The outsiders on the far side of a prohibited union. They are judged categorically — by national or cultic origin rather than individual disposition — and barred from the marriage regardless of sincerity, conduct, or willingness to join. They hold no seat in the deliberations that classify them; their only exits are renunciation of the relationship or assimilation into a community whose entry path the categorical reading keeps narrow and suspicious.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, prospective_intermarried_spouses, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(herem_command_dt7__durable_separation_reading, prospective_intermarried_spouses, excluded).

% Designated as contamination threats by origin rather than conduct. They bear categorical exclusion from covenant, alliance, and marriage; in the mandate's full historical form they bore sanctioned destruction of persons, cities, and cultic property. Nothing they individually do dissolves the category; only the boundary-administering community can reclassify them, and the categorical reading treats such reclassification as the breach it exists to prevent.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, non_covenant_outsider_populations, payer,
    powerless, generational, trapped, continental).

% Thinkers inside the tradition — prophetic universalists historically, modern moral philosophers and reform-minded clergy now — who argue that a categorical, perpetual separation mandate cannot bind. They pay in marginalization: denied teaching posts and platforms, their writings kept from communal education, their motives read as rebellion against divine authority rather than deliberation within it. Exit would mean leaving the tradition whose correction they seek.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, theological_dissenters_within_tradition, payer,
    moderate, generational, identity_locked, global).

% Academic biblical scholars, comparativists, and historians of religion who map how the mandate is read across traditions, document reception history and the recorded human costs of enforcement episodes, and publish analyses that none of the committed seats is obliged to treat as authoritative for practice.
narrative_ontology:constraint_stakeholder(herem_command_dt7__durable_separation_reading, hermeneutic_scholars, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(herem_command_dt7__durable_separation_reading, covenant_endogamous_households).
narrative_ontology:fixing_cost_class(herem_command_dt7__durable_separation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the minority-persistence problem: a small covenant community embedded in larger, assimilating cultures needs a legible membership boundary, a coordinated marriage field, and unambiguous allegiance lines to avoid dissolution. Bounded membership, endogamy, and categorical separation coordinate all three at once, and the divine-command form supplies an enforcement legitimacy that ordinary human authority has repeatedly failed to sustain across centuries.
% TRANSFER_FUNCTION: Moves marriage autonomy from covenant members (whose partner choice is confined to the boundary) into the custody of the boundary system itself; strips designated outsiders of moral standing and alliance eligibility (historically including land, property, and life under the destruction clauses); and moves adjudicatory jurisdiction over membership, marriage, and discipline to the administering clergy.
% ABSENT_VOICES: The designated outsiders are legislated about categorically and hold no seat anywhere in the tradition's self-governance — the texts define their status without their testimony, and the annihilation clauses remove the possibility of reply altogether. Within the community, members whose marriages are vetoed and the outside spouses they hoped to marry deliberate nowhere formally; dissenting theologians do speak, but the framework discounts their speech as rebellion rather than receiving it as deliberation.
% DISAPPEARANCE_RATIONALE: If the durable-separation mandate vanished overnight, endogamy would relax from obligation into preference, mixed households would be absorbed rather than ruptured, the clergy would lose jurisdiction over marriage and membership, and identity maintenance would shift to voluntary cultural transmission. The community's entire posture toward its neighbors — categorical and wall-like — would rearrange into negotiated, permeable coexistence, and the institutions constituted by gatekeeping would lose their warrant.
% FOUNDING_PROBLEM: A settlement-era tribal confederation faced extinction by assimilation: intermarriage and cultic absorption had visibly dissolved comparable small peoples, and the community's survival strategy demanded hard boundaries against the designated nations — demolish their cultic infrastructure, refuse their covenants, forbid intermarriage, so the covenant body would not dissolve into the peoples around it.
% FOUNDING_PROBLEM_CORROBORATION: Demographic sociology of minority persistence corroborates the underlying hazard from outside the benefiting parties: endogamy-retention rates, assimilation curves, and the documented dissolution of non-endogamous diaspora communities are independently attested phenomena, as is the settlement-era dissolution dynamic the command answered. What no outside source corroborates is the mandate's divine origin or the proportionality of its prescriptions — the corroboration covers the problem, not the remedy, and this story records that limit plainly.
narrative_ontology:disappearance_verdict(herem_command_dt7__durable_separation_reading, world_rearranges).
narrative_ontology:founding_problem_status(herem_command_dt7__durable_separation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(herem_command_dt7__durable_separation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(herem_command_dt7__durable_separation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(herem_command_dt7__durable_separation_reading, 0.82, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is high (0.82) because the arrangement's demands scale with the reading's own extension of them: every non-covenant person is permanently categorized, every member's marriage choice is permanently confined, and the mandate's violence-bearing clauses remain in force as warrants rather than expiring with their historical occasion. Suppression (0.78) is structural before it is procedural — the divine-command form converts questioning the mandate into rebelling against its author, which forecloses internal revision channels that ordinary human rules leave open; communal sanctions (shunning, disowning, denial of rites) then carry the load. Theater is low (0.18): within the enforcing communities the boundary maintenance does real work — marriages, memberships, and inheritances genuinely route through it — so performance is a thin overlay on functioning machinery. Accessibility_collapse (0.58) is partial by design of the analysis: inside the framework alternatives collapse nearly completely (assimilation or intermarriage equals apostasy), but outside it the alternatives plainly exist, and the collapse is enforced at the price of total identity loss. Resistance (0.62) is sustained and documented: prophetic universalist strands, historical contestation, modern ethical repudiation, and steady assimilatory defection. Claim and metrics are independent authored facts: I claim tangled_rope because the structure possesses a genuine coordination function (identity preservation solving a real minority-persistence problem) AND asymmetric extraction through the same structure, actively enforced — while the metrics describe high-intensity operation that may compute harsher at several seats. The measurement series runs on one shared grid (t=0..100 at intervals of 20) so every tracked metric is authored at every examined point.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently and that divergence is the datum. From the administrator seat, the arrangement is a coordination order the institution built, transmits, and legitimately polices — the closest thing to a rope view available inside the structure. From the outsider seats (non_covenant_outsider_populations, prospective_intermarried_spouses) the same structure computes as a snare: pure categorical extraction with no coordination benefit flowing to them and no exit from the category. The household seat straddles: net beneficiary across a lifetime, payer in every marriage decision. The autonomy seeker and the dissenter compute as targets with identity-fused exits — their constraint is not that alternatives are invisible but that every alternative costs the self. The engine derives these divergent classifications from the structural data; the authored claim does not adjudicate among them.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: covenant_boundary_administrators (collects jurisdiction, deference, and gatekeeping standing) and covenant_endogamous_households (collects continuity insurance, mutual aid, patrimonial integrity) — both derive low d, toward the subsidized end. Victims: non_covenant_outsider_populations and prospective_intermarried_spouses derive d near the full-target end (declared victims, trapped exit, powerless power); intermarriage_autonomy_seekers and theological_dissenters_within_tradition derive high d from their victim declarations, tempered somewhat by residual insider goods. Suppression is authored raw and unscaled; extractiveness is what the engine scales — and it scales upward here because the relevant scopes are continental-to-global (an origin-based category travels with the diaspora) and the exits are trapped or identity-locked rather than mobile. No directionality overrides are used: the beneficiary/victim declarations plus exit atoms already place every seat correctly, and the two dual-positioned cases are handled by secondary_role declarations rather than override arithmetic.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification is what prevents both standard misreadings. Reading the arrangement as a pure rope would erase the victims — it would treat categorical exclusion of origin-defined classes and the surrender of marriage autonomy as mere coordination overhead, which the victim declarations forbid. Reading it as a pure snare would erase the sincere coordination core — identity preservation through endogamy demonstrably solves a real collective-action problem, and the communities maintained by it are not cynical; the constraint's persistence is not maintained as cover. Mandatrophy is not resolved here: under this reading the founding problem (assimilatory dissolution) is asserted live, the arrangement continues to perform its original function, and no sunset clause exists — the mandate is presented as precisely the thing that must never sunset. The classification therefore tracks a functioning hybrid, not a decaying one; if the enforcement machinery ever atrophied while the mandate remained professed, theater_ratio would be the leading indicator and a piton drift would become visible.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment_structure,
    'This constraint is one reading of kernel herem_command_dt7 — what structurally changes if a sibling reading is adopted instead?',
    'Adoption of contextual_supersession_reading collapses the victim set to settlement-period populations and drives epsilon sharply downward (rope-or-scaffold-shaped residue with a sunset already passed); adoption of allegorical_displacement_reading removes behavioral constraint on real outsiders entirely and relocates the demand to internal moral discipline (near-mountain internal regimen). Resolution is by which reading a community''s authorities actually teach and enforce.',
    'The classification computed for this file holds only under the durable reading; under either sibling the beneficiary/victim structure, epsilon, and likely type all change. Cross-reading comparison must join on kernel_id, never merge on it.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment_structure, conceptual, 'Committer structure: this story is the durable_separation_reading seat of a three-reading kernel; sibling adoptions produce different constraints, not different measurements of this one.').

omega_variable(
    divine_origin_vs_construction,
    'Is the mandate a genuine transcendent requirement, or a human-authored boundary instrument attributed to divine authorship precisely to foreclose revision?',
    'Historical-critical analysis of the composition and redaction of Deuteronomy 7, comparative study of contemporaneous Near Eastern ban formulas, and reception-history evidence of the command''s utility to its human administrators.',
    'If constructed, the constraint reclassifies toward ordinary political boundary construction with human captors of the extraction (snare-leaning at most seats); if genuinely mandated, the measured extraction represents obedience cost rather than appropriable rent, and the coordination reading gains weight.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_origin_vs_construction, empirical, 'Whether the constraint''s anchor is transcendent or is human construction wearing transcendence as an enforcement multiplier.').

omega_variable(
    violence_component_status,
    'Does the durable reading carry the destruction clauses as live normative content, or retain them only as background warrant for separation while the operative demand is boundary discipline?',
    'Examine contemporary adherent teaching, juridical treatment of the destruction clauses, and enforcement practice: is the violent content preached as binding, filed as fulfilled-and-inert, or quietly dropped while officially affirmed?',
    'If live, suppression and extraction climb further and most seats compute snare; if inert, the constraint operates as boundary discipline carrying a residual legitimation debt, and the tangled_rope reading stands with lower tail risk.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(violence_component_status, empirical, 'Status of the mandate''s violent content under the durable reading as actually held.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression predominantly structural (sanctions, disowning, denial of rites) or internalized (members experience the boundary as their own identity and desire)?',
    'Post-exit trajectory studies of leavers: if boundary-enforcing attitudes persist after all structural sanctions are escaped, a substantial fraction is internalized; if compliance collapses with the sanctions, it was structural.',
    'If substantially internalized, effective suppression exceeds the structural measure — the constraint travels with its targets after exit, raising the true cost of the identity_locked exits and pushing target-seat classifications harsher; if structural, removing enforcement machinery would release the pressure quickly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Mechanism split of suppression: external sanction versus fused identity carrying the boundary from inside.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(herem_command_dt7__durable_separation_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(herem_dt7_durable_sep_tr_t0, herem_command_dt7__durable_separation_reading, theater_ratio, 0, 0.09).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_tr_t0, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_tr_t20, herem_command_dt7__durable_separation_reading, theater_ratio, 20, 0.11).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_tr_t20, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_tr_t40, herem_command_dt7__durable_separation_reading, theater_ratio, 40, 0.13).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_tr_t40, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_tr_t60, herem_command_dt7__durable_separation_reading, theater_ratio, 60, 0.15).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_tr_t60, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_tr_t80, herem_command_dt7__durable_separation_reading, theater_ratio, 80, 0.17).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_tr_t80, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_tr_t100, herem_command_dt7__durable_separation_reading, theater_ratio, 100, 0.18).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_tr_t100, observed).

% Extraction over time
narrative_ontology:measurement(herem_dt7_durable_sep_be_t0, herem_command_dt7__durable_separation_reading, base_extractiveness, 0, 0.64).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_be_t0, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_be_t20, herem_command_dt7__durable_separation_reading, base_extractiveness, 20, 0.69).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_be_t20, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_be_t40, herem_command_dt7__durable_separation_reading, base_extractiveness, 40, 0.73).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_be_t40, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_be_t60, herem_command_dt7__durable_separation_reading, base_extractiveness, 60, 0.77).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_be_t60, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_be_t80, herem_command_dt7__durable_separation_reading, base_extractiveness, 80, 0.8).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_be_t80, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_be_t100, herem_command_dt7__durable_separation_reading, base_extractiveness, 100, 0.82).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_be_t100, observed).

% Suppression requirement over time
narrative_ontology:measurement(herem_dt7_durable_sep_su_t0, herem_command_dt7__durable_separation_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_su_t0, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_su_t20, herem_command_dt7__durable_separation_reading, suppression_requirement, 20, 0.61).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_su_t20, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_su_t40, herem_command_dt7__durable_separation_reading, suppression_requirement, 40, 0.67).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_su_t40, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_su_t60, herem_command_dt7__durable_separation_reading, suppression_requirement, 60, 0.72).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_su_t60, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_su_t80, herem_command_dt7__durable_separation_reading, suppression_requirement, 80, 0.76).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_su_t80, observed).
narrative_ontology:measurement(herem_dt7_durable_sep_su_t100, herem_command_dt7__durable_separation_reading, suppression_requirement, 100, 0.78).
narrative_ontology:measurement_basis(herem_dt7_durable_sep_su_t100, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(herem_command_dt7__durable_separation_reading, identity_coordination).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__contextual_supersession_reading).
narrative_ontology:affects_constraint(herem_command_dt7__durable_separation_reading, herem_command_dt7__allegorical_displacement_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial label 'what the herem command means' decomposes into three structurally distinct constraints per the epsilon-invariance principle — durable_separation_reading (this file: timeless mandate, expansive victim set, high extraction on intermarriage autonomy and outsider standing), contextual_supersession_reading (temporally bounded, victim set closed with the settlement era, epsilon drops accordingly), and allegorical_displacement_reading (referent displaced inward, behavioral extraction on real outsiders removed). The decomposition exists because measuring 'the herem' one way yields negligible extraction and another way yields extreme extraction — that observable-dependence is the signature of multiple constraints sharing one label. This reading sits downstream of the shared textual kernel and cites its unqualified phrasing ('in every generation') as evidence; the sibling files carry their own epsilon, stakeholders, and classifications. Every family member links the others here; orphaning any one would hide the contamination path by which the durable reading's extraction figures borrow rhetorical force from the command's historical form.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
