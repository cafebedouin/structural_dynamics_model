% ============================================================================
% CONSTRAINT STORY: marriage_authority__judicial_harmonization_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__judicial_harmonization_reading, []).

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
 *   constraint_id: marriage_authority__judicial_harmonization_reading
 *   human_readable: Judicial Harmonization Pathway: Case-by-Constitutional-Floor Review Across Personal Law Codes
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   This story instantiates the judicial_harmonization_reading of the
 *   marriage_authority kernel: the arrangement under description is the
 *   standing practice by which the apex court, petition by petition, strikes
 *   down or reshapes provisions of the Hindu, Muslim, Christian, and Parsi
 *   family codes that fail constitutional scrutiny, producing incremental
 *   convergence toward a common rights floor without any enacted uniform
 *   civil code. The reading is institutional-mechanistic rather than strongly
 *   normative — it describes HOW authority is moving, not fully WHERE it
 *   should rest — and its own lights assess the arrangement as a
 *   genuine-but-hybrid coordination device: a real floor across communities,
 *   purchased with asymmetric transfers of normative authority. The epsilon
 *   referent is the standing arrangement itself (the case-by-case review
 *   pathway as it operates), never the enacted uniform code this pathway
 *   defers or the communal autonomy it displaces. Claim and metrics are
 *   independent authored facts: the claim is tangled_rope because the
 *   structure possesses all three canonical components — a genuine
 *   coordination function (common floor), asymmetric extraction (communal
 *   norm-authority flowing to the bench and risk-deferral flowing to the
 *   executive), and active enforcement (binding precedent, contempt, and
 *   post-2017 criminal backing). The metrics describe moderately extractive,
 *   increasingly theatrical, hardening-enforcement operation; the engine
 *   computes per-seat types from the structural data, and any divergence from
 *   the claim is the measurement the corpus exists to take. Sibling readings
 *   of the same kernel (communal autonomy, secularist, gender-rights,
 *   federalist-millet) are separate constraints in separate files; their
 *   structural deltas are recorded in the omega variables, not folded into
 *   this story.
 *
 * KEY AGENTS:
 *   - supreme_court_judiciary: agenda-setter and principal collector (institutional / identity_locked) — administers the floor, accrues interpretive authority, spends legitimacy defending it
 *   - individual_rights_claimants: intended beneficiary (moderate / constrained) — receives relief and precedent through litigation access
 *   - political_executive: incidental beneficiary (powerful / arbitrage) — collects risk-deferral, holds the unused legislative lever
 *   - traditional_religious_adjudicators: primary payer (organized / trapped) — boards and councils losing adjudicative authority with no exit from constitutional jurisdiction
 *   - conservative_community_members: diffuse payer (powerless / identity_locked) — absorb imposed reform without consent or exit
 *   - unrepresented_affected_women: excluded seat (powerless / trapped) — the unserved majority outside the courtroom
 *   - ucc_legislative_campaigners: excluded seat (organized / mobile) — locked out of the judicial pathway they compete with
 *   - law_commission_of_india: analytical observer (institutional / analytical) — studies and recommends, binds nothing
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.58).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.7).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.38).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.38).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization Pathway: Case-by-Constitutional-Floor Review Across Personal Law Codes").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, '4e9c4a60-7150-47f3-a018-a5a81d6e207d').
narrative_ontology:cs_kernel_codification('4e9c4a60-7150-47f3-a018-a5a81d6e207d', fixed_text).
narrative_ontology:cs_authority_grounding('4e9c4a60-7150-47f3-a018-a5a81d6e207d', lineage).
narrative_ontology:cs_interpretation_layer_present('4e9c4a60-7150-47f3-a018-a5a81d6e207d').
narrative_ontology:cs_reading_relation('4e9c4a60-7150-47f3-a018-a5a81d6e207d', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('4e9c4a60-7150-47f3-a018-a5a81d6e207d', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e9c4a60-7150-47f3-a018-a5a81d6e207d', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e9c4a60-7150-47f3-a018-a5a81d6e207d', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_axiom('4e9c4a60-7150-47f3-a018-a5a81d6e207d', foundational, judicial_floor_review_legitimate).
narrative_ontology:cs_axiom_status(judicial_floor_review_legitimate, holdable).
narrative_ontology:cs_axiom_grounding('4e9c4a60-7150-47f3-a018-a5a81d6e207d', judicial_floor_review_legitimate, conventional).
narrative_ontology:cs_axiom('4e9c4a60-7150-47f3-a018-a5a81d6e207d', foundational, incremental_convergence_avoids_communal_rupture).
narrative_ontology:cs_axiom_status(incremental_convergence_avoids_communal_rupture, holdable).
narrative_ontology:cs_axiom_grounding('4e9c4a60-7150-47f3-a018-a5a81d6e207d', incremental_convergence_avoids_communal_rupture, instrumental).
narrative_ontology:cs_reference_frame('4e9c4a60-7150-47f3-a018-a5a81d6e207d', directive_principle_gradual_realization).
narrative_ontology:cs_drift_state('4e9c4a60-7150-47f3-a018-a5a81d6e207d', contemporary_convergence_plateau, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e9c4a60-7150-47f3-a018-a5a81d6e207d', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, individual_rights_claimants).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, political_executive).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, traditional_religious_adjudicators).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, conservative_community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hears petitions attacking specific provisions of the Hindu, Muslim, Christian, and Parsi family codes, decides which survive fundamental-rights scrutiny, and issues binding precedents every subordinate court and community must follow. Each ruling extends the bench's writ into family-law territory no legislature has codified, and the accumulated precedent web commits the institution to defending and extending what it has built. The bench also spends legitimacy capital defending rulings against political and communal backlash and carries the docket burden of successive waves of challenges; stepping off the path would strand prior rulings and the litigants who rely on them.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, beneficiary).

% Women and minority members who litigate against discriminatory provisions — denied post-divorce maintenance, subjected to unilateral divorce, barred from equal inheritance or clerical marriage dissolution. A win delivers concrete relief and builds precedent for others in the same position; losing, or never reaching the apex court, leaves them under their community's code unchanged. Access runs through layers of lawyers, funded petitions, and years of appeals; no parallel forum offers an equivalent remedy.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, individual_rights_claimants, beneficiary,
    moderate, biographical, constrained, national).

% Governs without ever having to enact a unified family code: each judicial intervention lets it present reform as court-led while avoiding the electoral detonation a legislative bill would trigger across coalition politics. It retains the legislative route as a standing lever it declines to pull, and occasionally pulls it in reverse — statutorily overturning an unpopular ruling when backlash peaks, as after the 1985 maintenance decision.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, political_executive, beneficiary,
    powerful, biographical, arbitrage, national).

% Personal-law boards, clerical councils, and community tribunals whose interpretive authority over marriage, divorce, and maintenance erodes with each adverse ruling. They mobilize protest, issue resolutions and opinions, and lobby parliament for statutory reversal, but they cannot opt out of constitutional jurisdiction; their adjudicative relevance contracts whether or not they comply, and each compliance they do render under protest further ratifies the court's supervisory role.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, traditional_religious_adjudicators, payer,
    organized, generational, trapped, national).

% Adherents for whom the community's marriage norms are bound up with religious identity and inherited social order. Reforms arrive imposed from outside, without any intra-community deliberation or consent procedure; each ruling reads to them as dispossession of a law they did not agree to surrender. Leaving the community's normative world is not a live option for most, so the changes simply happen to them.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, conservative_community_members, payer,
    powerless, generational, identity_locked, national).

% The majority of women living under discriminatory provisions never litigate — no lawyer, no funds, no awareness that a remedy exists. Precedent-based reform reaches them decades late and unevenly, filtered through rulings they cannot invoke without the very access they lack. They would object that the pathway chiefly serves those already inside the courtroom.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, unrepresented_affected_women, excluded,
    powerless, biographical, trapped, national).

% Reform advocates pressing for a single enacted civil code covering all communities. The judicial pathway crowds them out: partial convergence drains urgency from the legislative project while leaving the plural structure intact, and the courts themselves repeatedly declare codification to be parliament's job alone. They would object that piecemeal review entrenches the very anomaly they exist to end.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, ucc_legislative_campaigners, excluded,
    organized, generational, mobile, national).

% Studies the personal-law question, conducted the 2018 national consultation on family-law reform, and publishes analysis of whether convergence should proceed by statute or by adjudication. It takes submissions from all communities and reform camps; its recommendations bind no one and it collects nothing from the arrangement either way.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, law_commission_of_india, observer,
    institutional, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Supplies a common minimum standard of marital rights — consent, maintenance, dignity, non-discrimination — across personal-law codes that otherwise diverge sharply, so that a citizen's basic civic protection does not depend on which community's law governs her.
% TRANSFER_FUNCTION: Moves normative authority over marriage from community religious traditions and the legislature to the apex court, ruling by ruling; moves material relief to successful litigants; and moves political risk away from elected officials onto judges.
% ABSENT_VOICES: Unrepresented affected women (no litigation access) would object that the pathway serves the courtroom-connected; conservative community members would object that no consent procedure ever reached them; legislative-code campaigners would object that partial convergence entrenches the anomaly; and the wider electorate never voted for courts to author family law. All four seats sit outside the mechanism's operating conversation.
% DISAPPEARANCE_RATIONALE: If the mechanism vanished overnight, hard-won protections (maintenance rights, the divorce-practice ban, decriminalized private choices) would lapse back to community codes for everyone without a personal judgment already in hand; pending litigants would be stranded mid-appeal; community boards would abruptly regain norm-space they have structured expectations around; and the executive would lose its deferral shield and face the legislative question directly. The family-law landscape would visibly rearrange.
% FOUNDING_PROBLEM: After independence, citizens' marital rights depended on which colonial-era religious code governed them; the constitution's directive principles called on the state to work toward a uniform code, but legislative consensus proved impossible in the shadow of partition-era communal politics, so the court began supplying a constitutional floor case by case instead.
% FOUNDING_PROBLEM_CORROBORATION: Gender-rights scholarship, Law Commission consultation records, and testimony of affected women attest the divergence-and-its-human-costs framing from outside the judiciary's and executive's seats. Communal-autonomy bodies and federalist-pluralism scholars dispute the framing itself, attesting that divergence is deliberate plural design rather than a defect. No source outside all benefiting parties certifies the founding problem as stated — which disagreement is precisely the kernel contest this reading sits inside.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.58, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__judicial_harmonization_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__judicial_harmonization_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness 0.58: the referent arrangement transfers real normative property (community law-authorship) without consent procedures, but also delivers substantive rights to identifiable claimants, so it is neither negligible nor predatory — a mid-high hybrid value. Suppression 0.70: persistence depends on active machinery — binding precedent, contempt jurisdiction, and since 2019 a criminal statute enforcing one landmark ruling — and communities have no jurisdictional exit; dissent is open but ineffective. Theater 0.38: operative holdings remain the core output, but a growing share of activity is signaling — annual exhortations that a uniform code 'desirably' awaits parliament, oral observations in hearings, virtue-display in reserved judgments — activity that manages legitimacy rather than deciding anything. Accessibility_collapse 0.5: alternatives (enacted code, voluntary community reform, state-level variation) remain visible and argued-for, but once a ruling lands, the foreclosed option cannot be revived by the community that lost it — partial, litigation-contingent collapse, unlike a natural law's total collapse. Resistance 0.65: sustained organized pushback — board campaigns, the 1986 statutory reversal, recurring private-member bills and counter-mobilizations — marks this as a defended construct, not an accepted fact. Temporal series run on ONE shared grid (t=0,8,16,24,32,36,40 over a 1985–2025 span: t0 the maintenance decision, t16 its statutory-reversal reinterpretation, t32 the divorce-practice and adultery-decriminalization rulings, t40 the post-same-sex-marriage-refusal plateau); extractiveness accumulates through t32 then dips slightly as the bench turns cautious, theater rises monotonically as signaling substitutes for stalled convergence, and suppression_requirement jumps at t32 when enforcement acquired criminal backing — an enforcement-hardening trajectory, which is why suppression_requirement is authored despite the static-picture scalar rule. The interval is CYCLICAL as well as trending: intervention, communal backlash, statutory reversal or recalibration, pause, next intervention. The oscillation is partly accountability feedback (the legislative lever disciplines the bench) and partly an extraction enabler — each cycle lets the bench probe a new limit at low per-move cost while the legislature absorbs the blame; the base_properties values are measured at interval end, the plateau phase after the most recent refusal-to-extend.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the bench's seat the arrangement is stewardship: a constitutional promise kept incrementally against political cowardice, with the costs (backlash, docket, legitimacy expenditure) borne proudly. From the claimant's seat it is protection: slow, exclusionary, but the only working remedy. From the adjudicator-board's seat it is piecemeal dispossession: authority taken ruling by ruling with no settlement, no compensation, and no exit. From the executive's seat it is insurance: someone else makes the hard calls. Same structure, four different lived types — the engine derives this divergence from the declared roles, exits, and directionalities; nothing in the authored claim adjudicates it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary/victim declarations drive the derivation: claimants (beneficiary, constrained exit) sit near the subsidized end; adjudicator-boards and conservative members (payers, trapped/identity_locked) sit near the full-target end, with identity lock pushing the conservative members furthest toward full-target since they carry the extraction into their self-concept. The executive (beneficiary, arbitrage exit) derives near the beneficiary end — correctly, since its gains are real and its costs optional. One override is declared: the institutional power atom is pinned to d=0.30 because the bench, though the principal collector, also bears substantial costs of the arrangement — legitimacy capital spent on backlash management, an ever-growing family-law docket, and precedent commitments it cannot shed — so a pure-beneficiary derivation (~0.1) would overstate its subsidy; 0.30 encodes collector-with-real-costs. Assumption flagged: the observer seat shares the institutional atom but is analytical-exited and should not feed the extraction computation; if the engine treats observers as directional agents, this override should be split per-agent in a future revision.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (divergence-dependent civic rights) is contested but not dead — the mechanism is still performing its function, actively, so no mandatrophy is declared and none is resolved. The drift vector to watch is theater: the signaling share has risen every period while convergence has plateaued, which is the classic early signature of a mechanism beginning to outlive its forward motion. If legislative deadlock proves permanent and the bench settles into exhortation-without-extension, the arrangement degrades toward administered performance — at which point the piton question (administrator-could-change-it, cost-asymmetry) becomes live, with the bench as agenda_setter and the diffuse community payers unable to force the fix. The tangled_rope classification prevents the opposite mislabeling too: reading the arrangement as pure extraction (as a strong communal-autonomy account would) erases the real floor it maintains; reading it as pure coordination (as the bench's own account does) erases the unpaid transfer of normative authority. The hybrid claim is the one the structure supports.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_positioning,
    'This constraint is one reading of the marriage_authority kernel — what would each sibling reading change structurally if instantiated instead?',
    'Compile the sibling stories and compare beneficiary/victim surfaces: communal_autonomy_reading deletes the bench''s beneficiary seat and re-derives the arrangement as confiscation of communal authority; secularist_reading moves agenda-setting to the legislature and re-dates the arrangement as transitional; gender_rights_reading re-references epsilon to intra-community equality and raises measured extraction from surviving patriarchal provisions; federalist_millet_reading restores fragmentation as designed and converts the floor''s spread into the cost term.',
    'Classification is reading-indexed: the same bench activity computes as hybrid coordination/extraction under this reading, pure extraction under communal autonomy, transitional scaffold under secularism, and deeper extraction under gender-rights. Cross-reading comparison is valid only through the family links, never by averaging epsilons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_positioning, conceptual, 'Committer-frame positioning: which kernel, which reading, where the sibling deltas land.').

omega_variable(
    transitional_vs_steady_state_pathway,
    'Is convergence-without-legislation a transitional bridge awaiting a statutory uniform code (sunset-shaped), or a steady-state governance mode with no terminal condition?',
    'Watch for an emergent terminal condition: enabling legislation, an explicit court declaration that harmonization is complete, or formal repudiation of the directive-principle destination. Absence of all three across successive review cycles indicates steady state.',
    'A credible sunset reclassifies the arrangement toward scaffold (with the sunset declared); indefinite persistence confirms tangled_rope with accumulating extraction and keeps the theater-ratio trend diagnostic.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transitional_vs_steady_state_pathway, empirical, 'Whether the mechanism is bridge or destination — the manifest seeded a scaffold hypothesis; the authored structure shows no sunset clause.').

omega_variable(
    consent_deficit_accumulation,
    'Does piecemeal imposition accumulate legitimacy debt among governed communities (rising resistance, falling compliance) or produce adaptive acceptance over generations?',
    'Longitudinal tracking of community-board mobilization intensity, compliance rates with adverse rulings, new-litigation volume, and intergenerational attitude surveys within affected communities.',
    'Accumulating debt pushes effective suppression up and risks enforcement decay or crisis-point reversal; adaptive acceptance stabilizes the hybrid and dampens the resistance metric.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(consent_deficit_accumulation, empirical, 'Whether the consent deficit compounds or washes out across cohorts.').

omega_variable(
    intra_community_distribution_ambiguity,
    'Are community members net beneficiaries (rights delivered) or net payers (autonomy lost) — and does the answer split by gender and generation?',
    'Disaggregate welfare outcomes by gender and cohort across the ruled-upon communities: litigation winnings, maintenance recovered, divorce-security gained, versus reported normative-loss and social-cost measures.',
    'If members are net beneficiaries, the victim declarations narrow to the institutions and the arrangement reads more coordination-heavy; if net payers, extraction deepens toward the snare boundary and the conservative-member seat dominates the directionality field.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intra_community_distribution_ambiguity, empirical, 'The distributional ambiguity inside the paying communities that a single victim label flattens.').

omega_variable(
    backlash_reciprocity_function,
    'Does legislative reversal capacity (the 1986 statutory override being the paradigm) discipline judicial extraction — making the mechanism reciprocally accountable — or merely punctuate an otherwise ratcheting process?',
    'Code each intervention-backlash cycle for net directional movement of the constitutional floor: count durable retractions versus re-expansions across the 1985–2025 span.',
    'If disciplining, the cycle caps effective extraction and the oscillation is accountability feedback; if ratcheting, each cycle nets extraction upward and the oscillation is itself an extraction mechanism (intermittent reinforcement), which would raise the long-run extraction estimate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(backlash_reciprocity_function, empirical, 'Function of the intervention-backlash cycle: brake or ratchet.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marriage_authority_jhr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(marriage_authority_jhr_tr_t8, marriage_authority__judicial_harmonization_reading, theater_ratio, 8, 0.18).
narrative_ontology:measurement(marriage_authority_jhr_tr_t16, marriage_authority__judicial_harmonization_reading, theater_ratio, 16, 0.22).
narrative_ontology:measurement(marriage_authority_jhr_tr_t24, marriage_authority__judicial_harmonization_reading, theater_ratio, 24, 0.27).
narrative_ontology:measurement(marriage_authority_jhr_tr_t32, marriage_authority__judicial_harmonization_reading, theater_ratio, 32, 0.33).
narrative_ontology:measurement(marriage_authority_jhr_tr_t36, marriage_authority__judicial_harmonization_reading, theater_ratio, 36, 0.35).
narrative_ontology:measurement(marriage_authority_jhr_tr_t40, marriage_authority__judicial_harmonization_reading, theater_ratio, 40, 0.38).

% Extraction over time
narrative_ontology:measurement(marriage_authority_jhr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(marriage_authority_jhr_be_t8, marriage_authority__judicial_harmonization_reading, base_extractiveness, 8, 0.49).
narrative_ontology:measurement(marriage_authority_jhr_be_t16, marriage_authority__judicial_harmonization_reading, base_extractiveness, 16, 0.52).
narrative_ontology:measurement(marriage_authority_jhr_be_t24, marriage_authority__judicial_harmonization_reading, base_extractiveness, 24, 0.56).
narrative_ontology:measurement(marriage_authority_jhr_be_t32, marriage_authority__judicial_harmonization_reading, base_extractiveness, 32, 0.63).
narrative_ontology:measurement(marriage_authority_jhr_be_t36, marriage_authority__judicial_harmonization_reading, base_extractiveness, 36, 0.61).
narrative_ontology:measurement(marriage_authority_jhr_be_t40, marriage_authority__judicial_harmonization_reading, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(marriage_authority_jhr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.42).
narrative_ontology:measurement(marriage_authority_jhr_su_t8, marriage_authority__judicial_harmonization_reading, suppression_requirement, 8, 0.46).
narrative_ontology:measurement(marriage_authority_jhr_su_t16, marriage_authority__judicial_harmonization_reading, suppression_requirement, 16, 0.5).
narrative_ontology:measurement(marriage_authority_jhr_su_t24, marriage_authority__judicial_harmonization_reading, suppression_requirement, 24, 0.54).
narrative_ontology:measurement(marriage_authority_jhr_su_t32, marriage_authority__judicial_harmonization_reading, suppression_requirement, 32, 0.66).
narrative_ontology:measurement(marriage_authority_jhr_su_t36, marriage_authority__judicial_harmonization_reading, suppression_requirement, 36, 0.68).
narrative_ontology:measurement(marriage_authority_jhr_su_t40, marriage_authority__judicial_harmonization_reading, suppression_requirement, 40, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, marriage_authority__federalist_millet_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: 'marriage authority in plural India' is a colloquial label covering five structurally distinct arrangements, one per reading of the kernel. Each sibling story carries its own epsilon over the SAME referent rule (the standing arrangement that reading contests) and its own beneficiary/victim structure: this file's moderate epsilon describes the judicial-mechanism arrangement assessed by the mechanism reading's own lights; the communal-autonomy sibling authors high epsilon for the same bench activity seen as confiscation; the secularist sibling authors a transitional profile awaiting legislative replacement; the gender-rights sibling re-references to intra-community equality and authors higher extraction from the patriarchal provisions the floor leaves standing; the federalist-millet sibling treats the floor's erosion of deliberate fragmentation as the cost. Upstream/downstream: this reading's judgments continuously shrink the operating space of the communal-autonomy and federalist-millet siblings (declared as influences edges), while coexisting with the secularist and gender-rights siblings as live rival/complementary positions. No sibling is folded into this file; the links route contamination-propagation and foreclosure analysis across the family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(marriage_authority__judicial_harmonization_reading, institutional, 0.3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
