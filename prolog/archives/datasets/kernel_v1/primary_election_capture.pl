% ============================================================================
% CONSTRAINT STORY: primary_election_capture
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_primary_election_capture, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: primary_election_capture
 *   human_readable: Primary Election Capture and Partisan Gatekeeping
 *   domain: political/electoral
 *
 * SUMMARY:
 *   Primary election capture describes the structural mechanism by which
 *   party establishments filter candidate access to primary ballots and
 *   debates, thereby controlling the set of options presented to primary
 *   voters. In the U.S. two-party system, primary winners are effectively
 *   guaranteed major general election ballot access, making primary
 *   gatekeeping a critical choke point for democratic candidate selection.
 *   The constraint exhibits tangled rope structure: the primary system
 *   provides genuine coordination benefits (aggregating party preferences,
 *   preventing ballot fragmentation) while simultaneously enabling extraction
 *   by the establishment (excluding non-aligned candidates, maintaining power
 *   concentration). The extractiveness has increased over the 40-year
 *   interval as digital organizing has reduced the technical justifications
 *   for ballot access barriers while parties have intensified gatekeeping
 *   mechanisms (higher debate thresholds, restrictive media coverage
 *   decisions, superdelegates). The theater ratio has risen as the stated
 *   technical rationales (ballot clutter prevention, signature verification)
 *   have become increasingly anachronistic with digital methods. Suppression
 *   requirement has increased as the establishment has deployed more
 *   sophisticated mechanisms: debate commission threshold engineering,
 *   mainstream media exclusion protocols, and donor network discipline.
 *
 * KEY AGENTS:
 *   - Party Establishment: Primary beneficiary (institutional/arbitrage) — controls candidate selection, maintains power concentration, benefits from gatekeeping enforcement
 *   - Donor Networks: Co-beneficiary (powerful/arbitrage) — benefits from concentrated candidate access, can direct resources to preferred candidates, gatekeeping reduces negotiating pressure from grassroots alternatives
 *   - Primary Voters Without Establishment Support: Primary victim (powerless/trapped) — cannot exit the constraint; choice set is pre-filtered; cannot directly influence candidate selection outside establishment-approved options
 *   - Non-Aligned Primary Candidates: Secondary victim (moderate/constrained) — face ballot access barriers, debate exclusion, media blackout; exit is theoretically possible but practically catastrophic (third-party stigma, no future major party path)
 *   - Grassroots Movement Coalitions: Organized secondary victim (organized/constrained) — can mobilize volunteers and small-dollar donors but face suppression; can exit through third-party formation but lose primary leverage
 *   - Wealthy Non-Aligned Candidates: Tertiary victim (powerful/mobile) — can bypass some gatekeeping through self-funding and direct media spending; less suppressed than resource-constrained outsiders
 *   - Electoral Administration System: Institutional actor maintaining the constraint (institutional/arbitrage) — enforces ballot access rules, administers debates (indirectly through commission structure), benefits from gatekeeping through power preservation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(primary_election_capture, 0.58).
domain_priors:suppression_score(primary_election_capture, 0.65).
domain_priors:theater_ratio(primary_election_capture, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(primary_election_capture, extractiveness, 0.58).
narrative_ontology:constraint_metric(primary_election_capture, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(primary_election_capture, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(primary_election_capture, tangled_rope).
narrative_ontology:human_readable(primary_election_capture, "Primary Election Capture and Partisan Gatekeeping").
narrative_ontology:topic_domain(primary_election_capture, "political/electoral").

domain_priors:requires_active_enforcement(primary_election_capture).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(primary_election_capture, party_establishment).
narrative_ontology:constraint_beneficiary(primary_election_capture, incumbent_aligned_candidates).
narrative_ontology:constraint_beneficiary(primary_election_capture, donor_networks).
narrative_ontology:constraint_victim(primary_election_capture, non_aligned_primary_voters).
narrative_ontology:constraint_victim(primary_election_capture, outsider_candidates).
narrative_ontology:constraint_victim(primary_election_capture, electoral_competition).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: PRIMARY VOTER WITHOUT ESTABLISHMENT SUPPORT (SNARE) — A voter in a district where the establishment candidate has locked the primary faces a choice constrained to pre-selected options. Cannot exit the constraint; cannot organize alternative primary pathways without massive grassroots capital. Experiences maximum extraction of democratic agency.
constraint_indexing:constraint_classification(primary_election_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: NON-ALIGNED PRIMARY CANDIDATE (SNARE) — Outsider candidate faces suppression through debate thresholds, ballot access rules, media blackout, and donor network exclusion. Exit is theoretically possible (third-party run) but practically catastrophic (splitting the vote, pariah status, no future viability within major parties). Extraction is severe and suppression is high — the constraint's enforcement machinery is explicitly designed to prevent this agent's advancement.
constraint_indexing:constraint_classification(primary_election_capture, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PARTY ESTABLISHMENT (ROPE) — Coordination mechanism from the establishment's perspective. The primary gatekeeping system solves the genuine collective action problem of aggregating party preferences and presenting a unified general election candidate. The establishment has full exit capacity (can change primary rules, accept insurgent candidates) but chooses not to — they experience the constraint as beneficial coordination that concentrates power in their hands.
constraint_indexing:constraint_classification(primary_election_capture, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: WEALTHY NON-ALIGNED CANDIDATE (TANGLED ROPE) — A well-funded outsider can bypass some gatekeeping mechanisms through direct media spending, self-funded campaigns, and grassroots organizational capacity. Experience is mixed: faces suppression and gatekeeping, but has enough resources to fight through primary blockades. Benefits from some coordination functions (ballot structure, debate infrastructure) while being extracted from by access barriers. Neither Snare (has exit through wealth) nor Rope (faces real extraction).
constraint_indexing:constraint_classification(primary_election_capture, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: GRASSROOTS MOVEMENT COALITION (TANGLED ROPE) — Organized agents (activist networks, issue-based coalitions, emerging party factions) face gatekeeping suppression but also benefit from the primary system's infrastructure, debate platforms, and ballot structure. Can organize donors and volunteers but lack the establishment's institutional machinery. Can exit through third-party formation but only at cost of losing future primary leverage. Experience is mixed extraction and coordination.
constraint_indexing:constraint_classification(primary_election_capture, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 6: FORMAL ELECTORAL ADMINISTRATION (PITON) — Primary rules, ballot access regulations, and debate commission structures are increasingly performative rather than functional. States maintain complex primary ballot access requirements (signatures, fees, filing deadlines) that claim to prevent ballot clutter but functionally serve gatekeeping. The rules persist through institutional inertia despite the rise of digital signature collection and online organizing that make the technical rationale obsolete. Theater ratio is high — the rules are maintained because the establishment benefits from gatekeeping, not because the stated technical justifications hold.
constraint_indexing:constraint_classification(primary_election_capture, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, primary gatekeeping might appear as an inherent structural feature of party-based democracy: parties must maintain some coherence, candidates must meet minimal qualification thresholds, debates cannot include unlimited participants. This perspective risks naturalizing what is actually a contingent institutional arrangement designed to concentrate power. The false summit detector will identify this as naturalization of a constructed constraint.
constraint_indexing:constraint_classification(primary_election_capture, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(primary_election_capture_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(primary_election_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(primary_election_capture, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(primary_election_capture, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(primary_election_capture, TR),
    TR >= 0.70.

:- end_tests(primary_election_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The party establishment captures substantial control over general election candidate pools by filtering primary access. However, the extraction is not total — wealthy outsiders can bypass some barriers, grassroots organizing can force establishment candidates to address outsider platforms, and primary voters can still nominate insurgent candidates if they mobilize sufficiently. The trajectory from 0.38 to 0.58 reflects intensifying gatekeeping over the interval as digital organizing threatened establishment control, prompting more sophisticated suppression (debate threshold engineering, media coordination). Suppression (0.65): High. Multiple enforcement mechanisms operate: ballot access requirements (signatures, fees, filing deadlines), debate commission thresholds set to exclude outsiders, mainstream media exclusion of non-establishment candidates, donor network discipline preventing contributions to non-aligned candidates. The suppression is not absolute (some pathways exist) but substantial. The trajectory reflects increasing enforcement sophistication as threats to establishment control have emerged. Theater ratio (0.68): Moderately high. Ballot access administrative rules (signature collection, fee systems) claim technical justifications (preventing ballot clutter, verifying candidate legitimacy) that have become increasingly anachronistic with digital methods. Debate commission threshold engineering is explicitly designed for gatekeeping but framed as quality assurance. The rising trajectory reflects growing gap between stated technical rationales and actual gatekeeping function. Claimed type (Tangled Rope): The constraint provides genuine coordination benefits (aggregating party preferences, preventing fragmentation of general election choices) while simultaneously enabling extraction (establishment control of candidate selection). The coordination function is real — without primary mechanisms, parties would face severe coordination problems. But the extraction is equally real — the establishment uses coordination infrastructure to exclude non-aligned candidates and maintain power concentration.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates maximal perspectival divergence. Party establishment sees rope (coordination function) and experiences arbitrage-level exit options — they can change primary rules anytime but choose not to. Non-aligned primary voters see snare (no exit from pre-filtered choices, trapped within major party options). Non-aligned candidates see snare (high suppression through multiple mechanisms). Wealthy non-aligned candidates see tangled rope (can partially bypass gatekeeping but face real barriers). Grassroots movements see tangled rope (benefit from primary infrastructure but face suppression). Electoral administration sees piton (rules persist through institutional inertia despite technical obsolescence). The analytical observer risks mountain perspective (naturalizing gatekeeping as inherent to party democracy) but structural data reveals this as false summit — the constraint is contingent institutional arrangement, not natural law.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is determined by structural position. Party establishment derives d from beneficiary status + arbitrage exit → low d → negative chi → experiences as rope coordination. Primary voters without support derive d from victim status + trapped exit → high d → high chi → experiences as snare extraction. Non-aligned candidates derive d from victim status + constrained exit (third-party run is catastrophic) → high d → high chi → experiences as snare. Wealthy non-aligned derive d from victim status + mobile exit (can self-fund) → lower d than resource-constrained outsiders → tangled rope. Grassroots movements derive d from victim status + constrained exit (can exit as third party but lose primary leverage) + organized power → moderate d → tangled rope. The perspectival gaps are driven by exit options (arbitrage vs trapped vs constrained vs mobile) differentiating how agents experience the same structural extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   PRIMARY ELECTION CAPTURE resolves mandatrophy through structural bifurcation. The constraint is legitimately Rope from the establishment's perspective (genuine coordination mechanism) and legitimately Snare from the non-aligned voter's perspective (extracted democratic agency with no exit). Both readings are structurally accurate — the establishment IS solving a coordination problem (aggregating party preferences), AND the excluded voter IS experiencing pure extraction (pre-filtered choices with no real alternative). The resolution is that these are not different perspectives on a single constraint; they are different structural dimensions of the same institutional arrangement. The establishment's coordination problem IS the mechanism by which the voter's extraction occurs. Recognizing this relationship prevents false mandatrophy resolution that would claim 'it's really just one type.' The constraint is Tangled Rope precisely because it is genuinely both coordination and extraction — the two functions are structurally inseparable.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    primary_vs_general_election_function,
    'Does primary gatekeeping serve a genuine coordination function (aggregating party preferences) or is coordination merely the cover story for extraction by the establishment?',
    'Comparative institutional analysis: do primary systems with lower gatekeeping barriers (more open access, lower ballot signatures, more debate slots) show degraded party coherence in general elections? Do they produce less viable general election candidates? Or do they produce more diverse, equally viable slates?',
    'If genuine coordination: the constraint is legitimately Tangled Rope (coordination with asymmetric extraction). If cover story: the constraint is more purely Snare (extraction disguised as party function). This determines whether the establishment''s rope-like experience is structural or self-serving.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(primary_vs_general_election_function, empirical, 'Whether primary gatekeeping serves genuine coordination or is extraction cover story').

omega_variable(
    ballot_access_technical_justification,
    'Are voter signature, filing fee, and deadline requirements still technically necessary for ballot administration, or have digital methods and modern organizing made the stated technical justifications obsolete?',
    'Systems analysis: jurisdictions that have liberalized ballot access rules (reduced signatures, digital filing, extended deadlines) and measured ballot management outcomes. Data on whether ballot clutter, voter confusion, or counting errors increase when access barriers fall.',
    'If technical justifications are obsolete: the rules are purely gatekeeping cover, elevating theater_ratio and reframing as piton. If technical justifications remain valid: the rules serve a genuine administrative function, supporting the rope perspective on coordination necessity.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(ballot_access_technical_justification, empirical, 'Technical necessity of ballot access administrative requirements').

omega_variable(
    establishment_party_coherence_mechanism,
    'What is the actual mechanism by which primary gatekeeping produces party coherence? Is it genuinely causal (gatekeeping ← coherence production) or is coherence driven by other factors (shared ideology, donor networks, electoral incentives) and gatekeeping merely selects for candidates already aligned?',
    'Counterfactual analysis: parties or party factions that have adopted open primary mechanisms (superdelegates abolished, lower ballot barriers, ranked-choice primaries). Do they lose coherence compared to baseline? Or do other mechanisms (party platform, donor alignment, media narrative) maintain coherence independently?',
    'If causal: gatekeeping is a necessary coordination mechanism (supports rope/tangled rope classification). If selection only: gatekeeping is pure filtering for establishment preference (supports snare classification, reduces rope justification).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(establishment_party_coherence_mechanism, empirical, 'Whether primary gatekeeping causes or merely selects for party coherence').

omega_variable(
    democratic_legitimacy_threshold,
    'At what level of primary access and openness does the democratic legitimacy of primary elections fall below the threshold needed to justify their gatekeeping role in general election access?',
    'Democratic theory literature + empirical polling: voter perception of primary legitimacy as primary access expands. Measurement of perceived fairness, representativeness, and voter turnout across jurisdictions with different primary openness levels.',
    'If threshold is crossed: primary gatekeeping becomes illegitimate as a filter for general election viability, reframing the constraint from coordination to pure extraction. This could elevate classification toward snare even from establishment perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(democratic_legitimacy_threshold, preference, 'Democratic legitimacy threshold for primary gatekeeping').

omega_variable(
    identity_locked_partisan_voter,
    'For voters strongly identified with a major party, does party primary gatekeeping function as structural suppression or as internalized identity lock? That is, do these voters experience the constraint as external coercion (trapped by rules) or as self-governance (accepting the constraint as part of their party identity)?',
    'Qualitative research: interviews and behavior analysis of strong partisans who oppose primary gatekeeping versus those who accept it. Measurement of how many voters perceive primary barriers as legitimate party functions versus illegitimate suppression. Post-exit behavior of voters who change party affiliation — does suppression perception change after identity shift?',
    'If identity-locked dominates: many powerless voters experience rope or piton rather than snare, because they have internalized the party''s gatekeeping as legitimate. The perspectival gap is partly cognitive rather than purely structural. If trapped dominates: the snare classification is accurate and the identity lock is a secondary capture mechanism.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_locked_partisan_voter, empirical, 'Identity lock versus structural suppression in partisan voters').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(primary_election_capture, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(pec_tr_t0, primary_election_capture, theater_ratio, 0, 0.52).
narrative_ontology:measurement(pec_tr_t20, primary_election_capture, theater_ratio, 20, 0.62).
narrative_ontology:measurement(pec_tr_t40, primary_election_capture, theater_ratio, 40, 0.68).

% Extraction over time
narrative_ontology:measurement(pec_be_t0, primary_election_capture, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(pec_be_t20, primary_election_capture, base_extractiveness, 20, 0.52).
narrative_ontology:measurement(pec_be_t40, primary_election_capture, base_extractiveness, 40, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(pec_su_t0, primary_election_capture, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(pec_su_t20, primary_election_capture, suppression_requirement, 20, 0.62).
narrative_ontology:measurement(pec_su_t40, primary_election_capture, suppression_requirement, 40, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(primary_election_capture, resource_allocation).
narrative_ontology:affects_constraint(primary_election_capture, general_election_ballot_access).
narrative_ontology:affects_constraint(primary_election_capture, donor_network_concentration).
narrative_ontology:affects_constraint(primary_election_capture, mainstream_media_gatekeeping).

% DUAL FORMULATION NOTE:
% Primary election capture is structurally upstream of general election access — primary winners are effectively guaranteed major party ballot access. The constraint family includes separate stories for ballot access administration (which has shifted from technical necessity to pure gatekeeping), donor network gatekeeping (which supplies enforcement mechanism), and mainstream media's role in establishing debate worthiness. All three are linked by their role in establishing and enforcing the primary capture mechanism.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(primary_election_capture, institutional, 0.08).
constraint_indexing:directionality_override(primary_election_capture, organized, 0.55).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
