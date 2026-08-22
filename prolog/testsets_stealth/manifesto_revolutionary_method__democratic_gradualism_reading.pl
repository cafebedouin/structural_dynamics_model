% ============================================================================
% CONSTRAINT STORY: manifesto_revolutionary_method__democratic_gradualism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-07-01
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_manifesto_revolutionary_method__democratic_gradualism_reading, []).

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
 *   constraint_id: manifesto_revolutionary_method__democratic_gradualism_reading
 *   human_readable: Democratic-Gradualist Channel for Working-Class Transformation
 *   domain: political philosophy / revolutionary theory / historical materialism
 *
 * SUMMARY:
 *   A standing arrangement channels working-class political power through
 *   electoral majorities, parliamentary institutions, and legally recognized
 *   unions: parties contest elections and legislate reform, unions bargain
 *   within statutory frameworks and mobilize voters, and the state processes
 *   the resulting conflict as procedure. The arrangement solves a real
 *   coordination problem — dispersed discontent becomes durable majorities
 *   and enforceable concessions — while concentrating agenda-setting,
 *   careers, and disciplinary power in party and union apparatuses, and
 *   marking extra-parliamentary methods as illegitimate. Claim and metrics
 *   are independent authored facts: claimed_type states tangled_rope from the
 *   structural data (genuine coordination plus asymmetric extraction plus
 *   active enforcement); the metrics describe observed operation. Per the
 *   epsilon-referent rule, extractiveness assesses THIS standing arrangement
 *   as this reading sees it — never the alternatives sibling readings would
 *   install.
 *
 * KEY AGENTS:
 *   - - social_democratic_parties: Agenda setter (institutional / identity_locked) — administers the channel, captures its gains, locks strategy to it
 *   - - institutional_trade_unions: Beneficiary and co-administrator (organized / constrained) — collects recognition and bargaining rights, enforces procedural discipline
 *   - - working_class_electorate: Coordinated base and net payer of initiative (organized / constrained) — lends energy between elections, receives scheduled concessions
 *   - - revolutionary_militants: Primary target (moderate / trapped) — bears expulsion, blacklisting, and the adventurist label
 *   - - wildcat_and_council_currents: Secondary target (moderate / trapped) — builds bypass structures that are refused recognition
 *   - - liberal_democratic_state: Structural beneficiary (institutional / identity_locked) — collects continuity: conflict processed as procedure
 *   - - disenfranchised_and_colonized_subjects: Excluded voice (powerless / trapped) — governed by the timetable, never enrolled in it
 *   - - labor_historians_and_left_critics: Analytical observer (analytical / analytical) — reconstructs the record from outside the apparatuses
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(manifesto_revolutionary_method__democratic_gradualism_reading, 0.42).
domain_priors:suppression_score(manifesto_revolutionary_method__democratic_gradualism_reading, 0.28).
domain_priors:theater_ratio(manifesto_revolutionary_method__democratic_gradualism_reading, 0.44).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 0.44).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(manifesto_revolutionary_method__democratic_gradualism_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(manifesto_revolutionary_method__democratic_gradualism_reading, tangled_rope).
narrative_ontology:human_readable(manifesto_revolutionary_method__democratic_gradualism_reading, "Democratic-Gradualist Channel for Working-Class Transformation").
narrative_ontology:topic_domain(manifesto_revolutionary_method__democratic_gradualism_reading, "political philosophy / revolutionary theory / historical materialism").

domain_priors:requires_active_enforcement(manifesto_revolutionary_method__democratic_gradualism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(manifesto_revolutionary_method__democratic_gradualism_reading, '5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb').
narrative_ontology:cs_kernel_codification('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', fixed_text).
narrative_ontology:cs_authority_grounding('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', lineage).
narrative_ontology:cs_interpretation_layer_present('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb').
narrative_ontology:cs_reading_relation('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', manifesto_revolutionary_method__vanguard_rupture_reading, coexists_with).
narrative_ontology:cs_reading_relation('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', manifesto_revolutionary_method__council_communist_reading, coexists_with).
narrative_ontology:cs_axiom('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', foundational, transition_requires_majority_democratic_consent).
narrative_ontology:cs_axiom_status(transition_requires_majority_democratic_consent, holdable).
narrative_ontology:cs_axiom_grounding('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', transition_requires_majority_democratic_consent, deontological).
narrative_ontology:cs_axiom('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', foundational, existing_structures_suffice_for_class_power).
narrative_ontology:cs_axiom_status(existing_structures_suffice_for_class_power, holdable).
narrative_ontology:cs_axiom_grounding('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', existing_structures_suffice_for_class_power, instrumental).
narrative_ontology:cs_reference_frame('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', parliamentary_road_to_socialism).
narrative_ontology:cs_drift_state('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', contemporary_neoliberal_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5d99dc6c-dafd-41ac-8cc6-bc6bfdc73dbb', '').
narrative_ontology:cs_kernel_id(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, institutional_trade_unions).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, wildcat_and_council_currents).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).
narrative_ontology:constraint_victim(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, gradualist_reform_doctrine).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, electoral_majority_legitimacy).
narrative_ontology:constraint_vindicates(manifesto_revolutionary_method__democratic_gradualism_reading, constitutional_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Contest elections at every level, select candidates, write programs, form governments or oppositions, and discipline members who pursue extra-parliamentary methods. Affiliation fees, public campaign finance, and patronage flow through them. Abandoning the electoral-parliamentary path would dissolve the party's reason for existence, so strategy is locked to the channel even when results disappoint.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties, agenda_setter,
    institutional, generational, identity_locked, national).

% Hold statutory bargaining rights, administer benefit funds, and deliver member mobilization to allied parties at election time. Legal recognition and access to corporatist consultation are conditional on enforcing contract peace and channeling grievances into procedure. Dropping out of the framework would forfeit recognition, funds, and members to rival or employer-dominated structures.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, institutional_trade_unions, beneficiary,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, institutional_trade_unions, agenda_setter).

% Votes, joins, pays dues, and withholds labor within legal limits; receives wages, protections, and services on the schedule legislation permits. Direct initiative such as general strikes, factory occupations, or parallel institutions carries legal risk and official condemnation, so most energy is lent to representatives between elections. Exit from wage dependence or from the polity is not available.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, payer,
    organized, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(manifesto_revolutionary_method__democratic_gradualism_reading, working_class_electorate, beneficiary).

% Organize for transformation outside or against the electoral-parliamentary path: factional newspapers, strike committees, insurrectionary networks. They face expulsion from unions and parties, blacklisting, surveillance, and prosecution at intervals, and are publicly framed as reckless adventurers endangering the movement's gains. Their organizing base lies wherever the channel's institutions reach, so avoidance is not possible.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, revolutionary_militants, payer,
    moderate, biographical, trapped, national).

% Build strike committees, shop-floor assemblies, and council experiments that bypass both union bureaucracy and parliament. Recognition is refused, strikes ruled unlawful, and leaders dismissed or jailed during episodes; between episodes the structures are starved of members who fear losing channel-mediated protections.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, wildcat_and_council_currents, payer,
    moderate, immediate, trapped, local).

% Administers the procedures the channel runs on and collects the payoff: class conflict arrives as petitions, contracts, and court cases instead of barricades. Concedes reforms when pressure demands, enforces order when pressure exceeds procedure. Its continuity is bound to the channel's operation; it cannot exit its own constitution.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, liberal_democratic_state, beneficiary,
    institutional, civilizational, identity_locked, national).

% Live under the order the channel stabilizes — colonial subjects, migrants, the unenfranchised — without a vote in its timetables. Reforms arrive last or never; their objections surface only episodically, when their own struggles force entry onto the metropolitan agenda.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, disenfranchised_and_colonized_subjects, excluded,
    powerless, generational, trapped, global).

% Study the channel's record from outside its apparatuses: archival reconstruction of expulsions and bargains, comparative assessment of reform output against stated aims. Hold no office in the channel and collect nothing from its operation.
narrative_ontology:constraint_stakeholder(manifesto_revolutionary_method__democratic_gradualism_reading, labor_historians_and_left_critics, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(manifesto_revolutionary_method__democratic_gradualism_reading, social_democratic_parties).
narrative_ontology:fixing_cost_class(manifesto_revolutionary_method__democratic_gradualism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of converting geographically dispersed, individually weak working-class discontent into durable legislative majorities, enforceable labor law, and funded welfare institutions: elections aggregate preferences, parties maintain programmatic continuity between contests, unions coordinate the withholding of labor within legal bounds, and parliaments convert majorities into statutes.
% TRANSFER_FUNCTION: Moves political initiative and agenda control upward from rank-and-file workers and insurgent movements to elected representatives, party leaderships, and union officials; moves material concessions (wages, hours, insurance, housing) from state budgets and employer margins to working-class constituencies on a timetable set by parliamentary arithmetic and capital's tolerance rather than by workers' direct action; moves careers, affiliation fees, and patronage to the apparatuses that run the channel.
% ABSENT_VOICES: The disenfranchised and colonized — those governed by metropolitan gradualism but never enrolled in its franchise or its timetable — would object that the schedule of patience was never theirs to grant; they stand outside the polity the channel presupposes. Future generations bearing the compounded costs of deferred transformation are likewise unrepresented. Within the metropole, expelled militants retain voice only as defendants.
% DISAPPEARANCE_RATIONALE: If the channel vanished overnight — if working-class politics no longer ran through elections, parties, and recognized unions — the welfare-labor settlement would lose its administering machinery; either insurrectionary and council forms would proliferate into the vacuum or class conflict would fragment into unmediated episodic struggle. Party apparatuses, union recognition regimes, and the state's conflict-processing routines would all rearrange.
% FOUNDING_PROBLEM: After the defeats of 1848 and the Paris Commune showed that insurrection fails against modern bureaucratic-military states, and with mass suffrage opening a lawful path, the founding problem was: can the working class take and transform power through the ballot and existing institutions, avoiding civil war?
% FOUNDING_PROBLEM_CORROBORATION: Corroborated as contested from outside the benefiting parties: the Bernstein-Luxemburg exchange and subsequent historiography of the Second International document the dispute at its origin; comparative political science on left governments facing capital countermeasures (investment strikes, coup threats) tests the ceiling empirically; documented critiques from council-communist and vanguard currents attest the failure reading. Attestation does not come from the channel's own parties and unions alone.
narrative_ontology:disappearance_verdict(manifesto_revolutionary_method__democratic_gradualism_reading, world_rearranges).
narrative_ontology:founding_problem_status(manifesto_revolutionary_method__democratic_gradualism_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(manifesto_revolutionary_method__democratic_gradualism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(manifesto_revolutionary_method__democratic_gradualism_reading, 0.42, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).
:- end_tests(manifesto_revolutionary_method__democratic_gradualism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.42 at interval end) because the channel genuinely delivers — welfare states, labor law, bargaining rights — while charging deferral costs, demobilizing direct initiative, and paying apparatus rents; the series shows extraction peaking in militant-suppression eras (1923) and accommodation eras (1990) and dipping in delivery eras (1945). Suppression is currently low-moderate (0.28): marginalization today is mostly soft (irrelevance, stigma, procedural exclusion) rather than coercive. Theater_ratio (0.44) rises steadily as the program-practice gap widens — transformative vocabulary retained over managed-capitalist practice. Accessibility_collapse (0.40) is low for a construct: alternatives remain visible and periodically attempted, which is precisely why the underlying kernel stays contested. Resistance (0.60) is high: revolutionary and council currents have opposed the channel continuously since the Second International split. All three tracked series run on one shared seven-point grid (1890-2020) so every metric is authored at every examined time point. The suppression_requirement series is authored because this story specifically traces enforcement-capacity change: the channel's disciplinary machinery (war-time discipline, paramilitary suppression of councils, Cold War union purges, anti-radical employment vetting) built up, peaked, and decayed — a dynamic picture the static scalar cannot carry. Suppression remains a raw structural property, unscaled by power or scope; only extractiveness is scaled downstream.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the party seat the channel is its own achievement and identity — a low-directionality agenda-setter experiences coordination-dominant operation. From the union seat it is a favorable bargain: protection exchanged for discipline. From the electorate seat it is mixed — real concessions against surrendered initiative. From the militant and wildcat seats the same structure operates as enforced exclusion and extraction — snare-flavored experience computed from trapped exit and victim position. The state seat collects pure continuity. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries map to low directionality: parties (concentrated capture of fees, careers, agenda control), unions (recognition and bargaining rights), and the state (conflict processed without rupture). Victims map to high directionality: militants and wildcat currents, both trapped — the channel's institutions reach everywhere they could organize, and their alternative is precisely what the enforcement machinery exists to refuse. The electorate sits nearer the target end than symmetric: it pays in surrendered initiative and receives concessions on a schedule it does not set, and it cannot exit wage dependence or the polity. No directionality overrides are used: the derivation from beneficiary/victim declarations plus exit options is adequate, and same-power-atom seats (unions and electorate, both organized) diverge by role and exit, which per-seat computation reads directly.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — whether ballots can carry transformation beyond welfare capitalism — is contested, not dead, so no mandatrophy resolution is declared; the (status=contested x verdict=world_rearranges) cell raises no zombie flag. The tangled_rope classification prevents mislabeling in both directions: a pure-snare reading would erase the real reforms, real participation, and real coordination the channel delivers; a pure-rope reading would erase the disciplined victims, the apparatus capture, and the enforcement needed to hold the boundary against extra-parliamentary methods. The drift watch is piton-ward: theater_ratio approaching 0.5 and the post-1990 hollowing (membership decline, program-practice gap) are atrophy symptoms; if the transformative function finishes atrophying while the form persists theatrically, the constraint migrates toward piton — the omega on reversibility tracks exactly this.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_indexicality,
    'This constraint is one reading of the manifesto_revolutionary_method kernel — the democratic_gradualism_reading. How would instantiating a sibling reading (vanguard_rupture_reading or council_communist_reading) restructure the constraint?',
    'Generate the sibling stories and compare structural data across readings: vanguard_rupture flips beneficiaries toward the party-state apparatus and victims toward deviated workers and class enemies; council_communist removes parliamentary beneficiaries entirely and centers workplace assemblies. The disagreement is located in the vehicle of working-class power: parliament versus party-state versus councils.',
    'Classification is reading-indexed: the same kernel yields different beneficiary/victim sets, different epsilon, and possibly different types per reading. Merging readings into one story would average epsilon across incompatible arrangements and violate epsilon-invariance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_indexicality, conceptual, 'Committer structure: this story is one indexed reading of a contested kernel; sibling readings are separate constraints.').

omega_variable(
    epsilon_phase_dependence,
    'Is the channel''s extractiveness a stable property, or does it swing between delivery eras (low felt extraction) and accommodation or suppression eras (high)?',
    'Longitudinal comparative analysis of reform output against concession extraction and initiative surrender per era, using the authored series as hypothesis and archival welfare-state and labor-law data as test.',
    'If phase-dependent, the single scalar understates peak-era extraction and the computed type may oscillate across eras (rope-leaning in delivery phases, snare-adjacent in suppression phases); if stable, the scalar is adequate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(epsilon_phase_dependence, empirical, 'Whether epsilon is era-stable or swings with the channel''s delivery/accommodation cycle.').

omega_variable(
    electoral_path_ceiling,
    'Can electoral majorities deliver transformation beyond welfare capitalism, or is there a structural ceiling — capital flight, investment strikes, constitutional and judicial constraints — that caps what the channel can produce?',
    'Comparative natural experiments: left governments that attempted structural transformation and met capital countermeasures (Chile 1970-73, France 1981-83, Greece 2015) — outcome patterns across cases test whether the ceiling is structural or contingent.',
    'A confirmed ceiling means the channel''s coordination function caps at welfare management, its extractive side (deferral, demobilization) dominates, and drift toward snare or piton accelerates; a refuted ceiling strengthens the rope reading and the founding problem''s liveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(electoral_path_ceiling, empirical, 'The Bernstein-Luxemburg question: whether the ballot has a structural ceiling.').

omega_variable(
    militant_suppression_mechanism,
    'Is the measured suppression of revolutionary militants structural (expulsions, blacklists, prosecution, refusal of recognition) or internalized (self-policing via the adventurism stigma, anticipatory conformity inside unions and parties)?',
    'Post-exit suppression trajectory: track militants and currents operating where the channel''s enforcement is weak or absent — if marginalization persists without enforcement, a substantial share is internalized.',
    'If substantially internalized, effective suppression exceeds the structural measure — targets carry the constraint''s discipline with them after leaving its institutions — and the victim seats'' computed extraction rises.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(militant_suppression_mechanism, empirical, 'Structural versus internalized share of militant suppression.').

omega_variable(
    piton_drift_reversibility,
    'Is the post-1990 theatrical thickening (rising theater_ratio, membership decline, program-practice gap) reversible through revival pressure, or is it terminal atrophy toward a maintained form without function?',
    'Trend analysis of membership, turnout, and the program-practice gap, plus the presence and strength of organized internal factions demanding refunctionalization versus purely careerist retention of the form.',
    'Reversibility sustains the tangled_rope classification indefinitely; irreversibility marks the transition toward piton — a constraint administered by those who could change it but whose fixing cost exceeds what they bear.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(piton_drift_reversibility, conceptual, 'Whether the channel''s late-interval drift is a revivable phase or terminal atrophy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(manifesto_revolutionary_method__democratic_gradualism_reading, 1890, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(demgrad_tr_t1890, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1890, 0.14).
narrative_ontology:measurement_basis(demgrad_tr_t1890, observed).
narrative_ontology:measurement(demgrad_tr_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1914, 0.2).
narrative_ontology:measurement_basis(demgrad_tr_t1914, observed).
narrative_ontology:measurement(demgrad_tr_t1923, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1923, 0.3).
narrative_ontology:measurement_basis(demgrad_tr_t1923, observed).
narrative_ontology:measurement(demgrad_tr_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1945, 0.24).
narrative_ontology:measurement_basis(demgrad_tr_t1945, observed).
narrative_ontology:measurement(demgrad_tr_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1968, 0.33).
narrative_ontology:measurement_basis(demgrad_tr_t1968, observed).
narrative_ontology:measurement(demgrad_tr_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement_basis(demgrad_tr_t1990, observed).
narrative_ontology:measurement(demgrad_tr_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, theater_ratio, 2020, 0.44).
narrative_ontology:measurement_basis(demgrad_tr_t2020, observed).

% Extraction over time
narrative_ontology:measurement(demgrad_be_t1890, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1890, 0.28).
narrative_ontology:measurement_basis(demgrad_be_t1890, observed).
narrative_ontology:measurement(demgrad_be_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1914, 0.34).
narrative_ontology:measurement_basis(demgrad_be_t1914, observed).
narrative_ontology:measurement(demgrad_be_t1923, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1923, 0.46).
narrative_ontology:measurement_basis(demgrad_be_t1923, observed).
narrative_ontology:measurement(demgrad_be_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1945, 0.36).
narrative_ontology:measurement_basis(demgrad_be_t1945, observed).
narrative_ontology:measurement(demgrad_be_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1968, 0.38).
narrative_ontology:measurement_basis(demgrad_be_t1968, observed).
narrative_ontology:measurement(demgrad_be_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 1990, 0.46).
narrative_ontology:measurement_basis(demgrad_be_t1990, observed).
narrative_ontology:measurement(demgrad_be_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, base_extractiveness, 2020, 0.42).
narrative_ontology:measurement_basis(demgrad_be_t2020, observed).

% Suppression requirement over time
narrative_ontology:measurement(demgrad_su_t1890, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1890, 0.18).
narrative_ontology:measurement_basis(demgrad_su_t1890, observed).
narrative_ontology:measurement(demgrad_su_t1914, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1914, 0.45).
narrative_ontology:measurement_basis(demgrad_su_t1914, observed).
narrative_ontology:measurement(demgrad_su_t1923, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1923, 0.66).
narrative_ontology:measurement_basis(demgrad_su_t1923, observed).
narrative_ontology:measurement(demgrad_su_t1945, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1945, 0.52).
narrative_ontology:measurement_basis(demgrad_su_t1945, observed).
narrative_ontology:measurement(demgrad_su_t1968, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1968, 0.56).
narrative_ontology:measurement_basis(demgrad_su_t1968, observed).
narrative_ontology:measurement(demgrad_su_t1990, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 1990, 0.34).
narrative_ontology:measurement_basis(demgrad_su_t1990, observed).
narrative_ontology:measurement(demgrad_su_t2020, manifesto_revolutionary_method__democratic_gradualism_reading, suppression_requirement, 2020, 0.28).
narrative_ontology:measurement_basis(demgrad_su_t2020, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(manifesto_revolutionary_method__democratic_gradualism_reading, resource_allocation).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__vanguard_rupture_reading).
narrative_ontology:affects_constraint(manifesto_revolutionary_method__democratic_gradualism_reading, manifesto_revolutionary_method__council_communist_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the revolutionary method debate' covers three structurally distinct arrangements: this electoral-parliamentary channel, the vanguard party-state, and federated workplace councils. Per the epsilon-invariance principle each is a separate story with its own epsilon, beneficiaries, and victims; this file instantiates the democratic-gradualist arrangement. Family links run through network.affects_constraints. The relations are authored coexists_with rather than influences because the pressure among readings is reciprocal rivalry — this reading's institutionalization shaped the siblings' operating environment (legality, union infrastructure, repression when in government) while their challenges reshaped it in turn — not one-way causation, and no reading's core premise logically forecloses another within a single framework.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
