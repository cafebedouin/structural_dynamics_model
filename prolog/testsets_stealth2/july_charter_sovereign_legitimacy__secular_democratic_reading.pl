% ============================================================================
% CONSTRAINT STORY: july_charter_sovereign_legitimacy__secular_democratic_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_july_charter_sovereign_legitimacy__secular_democratic_reading, []).

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
 *   constraint_id: july_charter_sovereign_legitimacy__secular_democratic_reading
 *   human_readable: July Charter — Secular-Democratic Reading (Civilian Supremacy Mandate)
 *   domain: constitutional_law/political_transitions
 *
 * SUMMARY:
 *   Following the 2024 uprising, the July Charter emerged as a negotiated
 *   settlement among secular-democratic, nationalist, and Islamist
 *   signatories, with implementation pending referendum and constitutional
 *   entrenchment. 'What the Charter mandates' is a contested kernel with
 *   three declared readings; this story instantiates ONE of them — the
 *   secular-democratic reading, under which the Charter establishes
 *   religiously neutral, electorally accountable institutions and
 *   subordinates the armed forces to civilian authority. Under this reading
 *   the arrangement solves a real coordination problem (a single legitimate
 *   sovereign authority after revolutionary rupture) while imposing
 *   asymmetric, actively enforced costs on two actor classes: Islamist
 *   political organization (Jamaat-e-Islami and its mass base) and the
 *   military's autonomous political authority. The claim and metrics are
 *   independent authored facts: claimed_type is tangled_rope because the
 *   structure carries both a genuine coordination function and asymmetric
 *   extraction requiring enforcement; the metrics describe the arrangement's
 *   actual operation without being tuned to that claim. Sibling readings
 *   (guided_nationalism_reading, military_custodian_reading) are separate
 *   constraint files linked through network.affects_constraints; their
 *   epsilon and victim sets differ because the readings differ, not because
 *   this constraint is measured differently.
 *
 * KEY AGENTS:
 *   - civilian_elected_government: Agenda-setting administrator (institutional/constrained) — runs the settlement, collects the authority the military cedes and the political space Islamist constraint opens
 *   - supreme_judiciary: Agenda-setting enforcer (institutional/constrained) — adjudicates the mandate's reach; its rulings decide whether subordination and secular limits are lived or nominal
 *   - secular_democratic_parties: Primary beneficiary (organized/mobile) — compete in a framework that neutralizes religiously-defined rivals and bars military veto
 *   - urban_liberal_civil_society: Identity-locked beneficiary (moderate/identity_locked) — supplies the settlement's drafters and defenders; professional identity fused to the project
 *   - religious_minority_communities: Trapped beneficiary (powerless/trapped) — formal equal citizenship under the secular guarantee; safety tracks enforcement
 *   - july_generation_activists: Identity-locked beneficiary (organized/identity_locked) — the uprising's student leaders; the Charter is the monument to their dead
 *   - jamaat_e_islami: Primary payer (organized/identity_locked) — mass Islamist party whose route to office the mandate narrows; exit is ideological, not logistical
 *   - islamist_mass_base: Diffuse payer (moderate/constrained) — voters whose preferred platform has no unconstrained vehicle under the mandate
 *   - military_autonomous_authority: Institutional payer (institutional/constrained) — loses budgetary discretion, promotion autonomy, and historical veto; force-capacity is resistance, not exit
 *   - international_democracy_partners: Analytical observer (institutional/analytical) — condition aid and recognition on the transition's credibility; bear none of the internal costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.48).
domain_priors:suppression_score(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.55).
domain_priors:theater_ratio(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0.3).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(july_charter_sovereign_legitimacy__secular_democratic_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(july_charter_sovereign_legitimacy__secular_democratic_reading, tangled_rope).
narrative_ontology:human_readable(july_charter_sovereign_legitimacy__secular_democratic_reading, "July Charter — Secular-Democratic Reading (Civilian Supremacy Mandate)").
narrative_ontology:topic_domain(july_charter_sovereign_legitimacy__secular_democratic_reading, "constitutional_law/political_transitions").

domain_priors:requires_active_enforcement(july_charter_sovereign_legitimacy__secular_democratic_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(july_charter_sovereign_legitimacy__secular_democratic_reading, '8989fda0-e625-4038-a953-7898364bb57a').
narrative_ontology:cs_kernel_codification('8989fda0-e625-4038-a953-7898364bb57a', fixed_text).
narrative_ontology:cs_authority_grounding('8989fda0-e625-4038-a953-7898364bb57a', distributed).
narrative_ontology:cs_reading_relation('8989fda0-e625-4038-a953-7898364bb57a', july_charter_sovereign_legitimacy__guided_nationalism_reading, forecloses).
narrative_ontology:cs_reading_relation('8989fda0-e625-4038-a953-7898364bb57a', july_charter_sovereign_legitimacy__military_custodian_reading, forecloses).
narrative_ontology:cs_axiom('8989fda0-e625-4038-a953-7898364bb57a', foundational, popular_sovereignty_religiously_neutral).
narrative_ontology:cs_axiom_status(popular_sovereignty_religiously_neutral, holdable).
narrative_ontology:cs_axiom_grounding('8989fda0-e625-4038-a953-7898364bb57a', popular_sovereignty_religiously_neutral, deontological).
narrative_ontology:cs_axiom('8989fda0-e625-4038-a953-7898364bb57a', foundational, military_subordinate_to_civilian_authority).
narrative_ontology:cs_axiom_status(military_subordinate_to_civilian_authority, holdable).
narrative_ontology:cs_axiom_grounding('8989fda0-e625-4038-a953-7898364bb57a', military_subordinate_to_civilian_authority, conventional).
narrative_ontology:cs_axiom('8989fda0-e625-4038-a953-7898364bb57a', secondary, defensive_secularism_regulates_political_religion).
narrative_ontology:cs_axiom_status(defensive_secularism_regulates_political_religion, holdable).
narrative_ontology:cs_axiom_grounding('8989fda0-e625-4038-a953-7898364bb57a', defensive_secularism_regulates_political_religion, instrumental).
narrative_ontology:cs_reference_frame('8989fda0-e625-4038-a953-7898364bb57a', secular_civilian_supremacy_settlement).
narrative_ontology:cs_drift_state('8989fda0-e625-4038-a953-7898364bb57a', pre_entrenchment_interim_period, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8989fda0-e625-4038-a953-7898364bb57a', '').
narrative_ontology:cs_kernel_id(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_elected_government).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_liberal_civil_society).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_minority_communities).
narrative_ontology:constraint_beneficiary(july_charter_sovereign_legitimacy__secular_democratic_reading, july_generation_activists).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_mass_base).
narrative_ontology:constraint_victim(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Wins office under the Charter's rules and administers the settlement: commands the armed forces through the constitutional chain, sets policy within secular limits, and staffs the institutions the Charter creates. Collects the authority the military cedes and the political space opened by constraining Islamist competitors. Leaving the arrangement would mean dismantling the settlement it presides over, at the cost of the legitimacy that elected it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_elected_government, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_elected_government, beneficiary).

% Interprets and applies the Charter's mandates — adjudicating disputes over party eligibility, military jurisdiction, and the boundary between secular law and religious practice. Its rulings determine whether the mandate is operative or declaratory. It cannot step outside its interpretive role without surrendering the review authority the settlement assigns it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, supreme_judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Compete for office in a framework that neutralizes their religiously-defined rivals and bars military veto over electoral outcomes. Larger non-Islamist parties retain credible campaigns under rival readings of the Charter, giving them options most seats lack; smaller secular-left parties depend on this framework exclusively.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, secular_democratic_parties, beneficiary,
    organized, biographical, mobile, national).

% Supplies the constitutional lawyers, journalists, and organizers who drafted and defend the settlement. Their professional and moral identity was built across decades of opposition work and is constituted by the secular-democratic project; abandoning it would dissolve the self-conception, not just a preference.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, urban_liberal_civil_society, beneficiary,
    moderate, generational, identity_locked, national).

% Hindu, Buddhist, Christian, and Ahmadiya communities receive formal equal-citizenship protection under the secular guarantee. Their physical safety tracks how thoroughly the settlement is enforced; emigration is costly and homeland attachment binds them to the outcome whatever it turns out to be.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, religious_minority_communities, beneficiary,
    powerless, generational, trapped, national).

% Student leaders of the uprising who paid in lives and prison terms for the fall of the previous order. The Charter is the monument to their dead; treating it as negotiable would betray killed and maimed comrades and invalidate their sacrifice, fusing their identity to the settlement's survival.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, july_generation_activists, beneficiary,
    organized, biographical, identity_locked, national).

% A mass Islamist party whose organizational network, student wing, and welfare institutions predate the republic. The secular-democratic mandate narrows its path to office and exposes its wartime-era record to tribunal process. It cannot become a secular party without ceasing to be itself; its lack of exit is ideological, not logistical.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, jamaat_e_islami, payer,
    organized, generational, identity_locked, national).

% Rural and small-town voters whose preferred platform — a larger place for religious law in public life — has no unconstrained vehicle under the mandate. They bear the arrangement diffusely: votes cast for a narrowed option, candidates screened by eligibility rules they did not write, grievances with no institutional address.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, islamist_mass_base, payer,
    moderate, biographical, constrained, national).

% The officer corps as an institution: it loses budgetary discretion, promotion autonomy, immunity from civilian prosecution, and the historical veto exercised through repeated interventions. Corporate cohesion survives; autonomous political authority is what the mandate removes. Its capacity to break the settlement by force is a resistance resource, not an exit.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, military_autonomous_authority, payer,
    institutional, generational, constrained, national).

% Foreign governments and multilateral bodies that condition aid, trade preferences, and recognition on the transition's credibility. They observe whether civilian command and secular guarantees are honored and can shift resources accordingly, but they bear none of the settlement's internal costs and cannot vote in it.
narrative_ontology:constraint_stakeholder(july_charter_sovereign_legitimacy__secular_democratic_reading, international_democracy_partners, observer,
    institutional, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(july_charter_sovereign_legitimacy__secular_democratic_reading, civilian_elected_government).
narrative_ontology:fixing_cost_class(july_charter_sovereign_legitimacy__secular_democratic_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the post-revolution sovereign-authority problem: establishes one legitimate, electorally accountable, religiously neutral source of command, ending open competition among elected civilians, the officer corps, and religiously-defined movements over who rules.
% TRANSFER_FUNCTION: Moves political authority and participation rights from Islamist political organization and autonomous military power toward civilian elected institutions; moves security of citizenship toward religious minorities; moves accountability exposure toward wartime-era collaborators and former ruling elites.
% ABSENT_VOICES: Hardline Islamist constituencies outside Jamaat's negotiating umbrella and rank-and-file soldiers with no political voice had no seat at the table. Within the room, Jamaat and military representatives argued their readings, but the drafting consensus was weighted toward the uprising's secular-democratic bloc; the unanimity of the signed text partly reflects who was seated, not absence of dissent.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, the settlement's three constituencies would return to open contest: the military would reclaim custodial prerogatives, Islamist parties would pursue religious-sovereignty legislation, minorities would face unprotected status, and the uprising's institutional gains would dissolve back into raw bargaining power among armed and organized actors.
% FOUNDING_PROBLEM: After the uprising toppled a decade of authoritarian rule, the victorious coalition needed a settlement that would prevent relapse into either military tutelage or religiously-defined sovereignty while holding secular, nationalist, and Islamist signatories together long enough to entrench a constitution.
% FOUNDING_PROBLEM_CORROBORATION: Jamaat-e-Islami's continuing insistence on renegotiating the sovereignty clauses and the military's public reservations about civilian trial jurisdiction attest, from outside the benefiting parties, that the founding problem — containing both actors — remains unresolved. Independent international transition-monitoring reports corroborate the fragility of the settlement from a third seat.
narrative_ontology:disappearance_verdict(july_charter_sovereign_legitimacy__secular_democratic_reading, world_rearranges).
narrative_ontology:founding_problem_status(july_charter_sovereign_legitimacy__secular_democratic_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(july_charter_sovereign_legitimacy__secular_democratic_reading, 'none', 1).
narrative_ontology:epsilon_provenance(july_charter_sovereign_legitimacy__secular_democratic_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(july_charter_sovereign_legitimacy__secular_democratic_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(july_charter_sovereign_legitimacy__secular_democratic_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is moderate (0.48 at interval end) rather than high because most participants are net beneficiaries and the transferred goods are political authority and participation rights, not material rents — but it is clearly non-zero because two actor classes bear concentrated, identity-relevant costs. Suppression (0.55) reflects the enforcement machinery the settlement requires: eligibility administration, tribunal process, civilian-command enforcement against a corps with coup capacity. Theater ratio (0.30) is low-to-moderate: the coordination functions are real, but a growing share of activity is ceremonial reaffirmation of the settlement while its hardest provisions await enforcement. Accessibility collapse (0.62) is partial: within the Charter's legal order the rival readings collapse, but they persist as live political positions outside it — hence below the ~0.85 mountain band. Resistance (0.58) is substantial and rising: Jamaat mobilization, military institutional pushback, and the latent possibility of an anti-settlement coalition between the two payer classes. The measurement series run on one shared time grid (years since Charter signing, points 0/2/4/6/8/10) so every tracked metric is authored at every examined point; t=0 is observed, later points are projected and flagged as such. Suppression_requirement is tracked because the story specifically traces enforcement machinery maturing during implementation; the scalar base_properties.suppression is the raw structural property and is never scaled by power or scope — only extractiveness is scaled, by directionality and scope, in the engine's computation.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute very differently. From the civilian_elected_government and secular_democratic_parties seats the arrangement presents as the founding coordination they fought for — near-beneficiary directionality, low experienced burden. From the jamaat_e_islami seat the same structure presents as targeted closure of its political existence: ideologically identity-locked (its religious-political identity IS its platform; becoming secular dissolves the agent), so it experiences maximal extraction with no exit. From the military_autonomous_authority seat the structure is a direct institutional demotion — it retains force capacity, which converts into resistance rather than exit. From the religious_minority_communities seat the identical provisions read as protection, near-full subsidy. The july_generation_activists and urban_liberal_civil_society seats are identity-locked beneficiaries: their self-conception is constituted by the settlement, so even its costs register as duties. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: civilian_elected_government (agenda-setter and residual claimant of transferred authority), secular_democratic_parties, urban_liberal_civil_society, religious_minority_communities, and july_generation_activists all sit near the subsidized end. Victim declarations drive high directionality: jamaat_e_islami and islamist_mass_base bear the participation constraint; military_autonomous_authority bears the subordination constraint. One override is declared: the derivation tends to damp directionality for institutional-power actors, but the military's relationship to THIS constraint is full-target on the autonomy dimension regardless of its power — the mandate aims precisely at what it holds — so d is overridden to 0.85 for the institutional power atom. Scope is national throughout except the observer seat (global), keeping the scope amplification modest and uniform across domestic seats.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing relapse into either military tutelage or religiously-defined sovereignty while holding a fractious signatory coalition together — is still live, corroborated from outside the benefiting parties by Jamaat's continuing renegotiation demands and the military's public reservations. Status=live combined with disappearance_verdict=world_rearranges produces no zombie flag: the arrangement persists because its problem persists, not because anyone is performing a dead function. The classification guards against mislabeling in both directions: calling this a pure rope would erase the real, concentrated costs borne by the Islamist and military seats; calling it a snare would erase the genuine coordination function (a single legitimate sovereign authority) that even the paying seats implicitly rely on for order. The tangled-rope reading keeps both facts on the table. Fixing cost is prohibitive: reopening the settlement requires the same cross-bloc consensus that produced it, and every signatory bloc's identity is now partially fused to the text it signed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_allocation,
    'Which reading does the July Charter''s text actually mandate — secular-democratic civilian supremacy, guided nationalism, or military custodianship? This story instantiates only the secular-democratic reading; the sibling readings are separate constraints with different epsilon, beneficiary sets, and victim sets.',
    'Referendum outcome on Charter implementation, the entrenchment clause''s final wording, and subsequent Supreme Court interpretation of the sovereignty and defense articles.',
    'If guided nationalism prevails, the victim set inverts (secular liberals and religious minorities become the constrained seats); if military custodianship prevails, elected civilians become the constrained seat. This story''s entire directional structure flips with the allocation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_allocation, conceptual, 'Committer-frame uncertainty: one kernel, three mutually exclusive readings; this file is one of them.').

omega_variable(
    exclusion_mechanism_for_political_islam,
    'Is Jamaat-e-Islami''s constrained position produced by deliberate structural exclusion (eligibility screens, deregistration exposure, tribunal process) or by ordinary application of neutral democratic rules it loses under?',
    'Comparative analysis of the Charter''s eligibility and accountability provisions against rules applied to all parties; litigation records showing whether screening instruments target doctrinal content or conduct.',
    'Deliberate exclusion concentrates extraction on the Jamaat seat and raises effective extraction sharply; neutral-rule losing distributes costs diffusely and lowers the seat''s directionality toward the symmetric range.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(exclusion_mechanism_for_political_islam, empirical, 'Whether the political-Islam victim set reflects targeted exclusion or ordinary democratic defeat.').

omega_variable(
    military_subordination_enforceability,
    'Can civilian authority actually enforce subordination — budget control, promotion authority, prosecutable misconduct — or does the mandate remain nominal while the officer corps retains de facto autonomy?',
    'Budget-line and appointment records across the implementation window; disciplinary cases brought and concluded under civilian jurisdiction.',
    'If enforcement is nominal, the military seat bears little real cost (theater rises, extraction on that seat falls) and the settlement''s coordination claim weakens; if enforced, extraction on the military seat is real and the settlement''s coordination function is confirmed.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(military_subordination_enforceability, empirical, 'Whether civilian supremacy is operative or declaratory.').

omega_variable(
    entrenchment_depth_of_charter_mandate,
    'Is the Charter''s secular-democratic mandate entrenched against majoritarian revision, or revisable by an elected majority hostile to it?',
    'Text of the implementation schedule and any supermajority or referendum locks; first attempted amendment''s procedural fate.',
    'Deep entrenchment makes the arrangement a durable settlement whose costs bind future majorities (raising long-run extraction on excluded seats); shallow entrenchment makes it a transient electoral outcome that a future coalition can unwind cheaply.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(entrenchment_depth_of_charter_mandate, conceptual, 'Constitutional durability of the mandate determines whose costs are locked in.').

omega_variable(
    secularism_scope_ambiguity,
    'Does ''secular'' in this reading mean a neutral public sphere in which religious expression is free, or an actively enforced separation that restricts religiously-defined political organization?',
    'Judicial gloss on the secularism clause in its first contested applications; drafting-history evidence of the clause''s intended breadth.',
    'Neutral-sphere scope shrinks the victim set dramatically (Jamaat is constrained only by ordinary electoral competition); enforced-separation scope widens it (religious association itself becomes regulated), raising both extraction and suppression on the Islamist seats.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(secularism_scope_ambiguity, conceptual, 'Definitional breadth of the secular mandate determines who counts as constrained.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(july_charter_sovereign_legitimacy__secular_democratic_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(jul_charter_secular_rd_tr_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 0, 0.16).
narrative_ontology:measurement(jul_charter_secular_rd_tr_t2, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 2, 0.19).
narrative_ontology:measurement(jul_charter_secular_rd_tr_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 4, 0.22).
narrative_ontology:measurement(jul_charter_secular_rd_tr_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 6, 0.25).
narrative_ontology:measurement(jul_charter_secular_rd_tr_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 8, 0.28).
narrative_ontology:measurement(jul_charter_secular_rd_tr_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(jul_charter_secular_rd_be_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 0, 0.36).
narrative_ontology:measurement(jul_charter_secular_rd_be_t2, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 2, 0.39).
narrative_ontology:measurement(jul_charter_secular_rd_be_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 4, 0.42).
narrative_ontology:measurement(jul_charter_secular_rd_be_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 6, 0.44).
narrative_ontology:measurement(jul_charter_secular_rd_be_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 8, 0.46).
narrative_ontology:measurement(jul_charter_secular_rd_be_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, base_extractiveness, 10, 0.48).

% Suppression requirement over time
narrative_ontology:measurement(jul_charter_secular_rd_su_t0, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 0, 0.38).
narrative_ontology:measurement(jul_charter_secular_rd_su_t2, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 2, 0.41).
narrative_ontology:measurement(jul_charter_secular_rd_su_t4, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 4, 0.45).
narrative_ontology:measurement(jul_charter_secular_rd_su_t6, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 6, 0.48).
narrative_ontology:measurement(jul_charter_secular_rd_su_t8, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 8, 0.52).
narrative_ontology:measurement(jul_charter_secular_rd_su_t10, july_charter_sovereign_legitimacy__secular_democratic_reading, suppression_requirement, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(july_charter_sovereign_legitimacy__secular_democratic_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__guided_nationalism_reading).
narrative_ontology:affects_constraint(july_charter_sovereign_legitimacy__secular_democratic_reading, july_charter_sovereign_legitimacy__military_custodian_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'the July Charter settlement' covers three structurally distinct claims that cannot share one epsilon. The secular-democratic reading (this file) constrains Islamist political organization and military autonomy; the guided-nationalism reading constrains secular-liberal and minority claims instead; the military-custodian reading constrains elected civilian authority instead. Each reading has its own victim set, its own beneficiaries, and its own epsilon; forcing them into one story would make epsilon observer-relative, which the chi formula forbids. The files are linked pairwise through affects_constraints; whichever reading wins entrenchment becomes the upstream constraint whose operation reshapes the viability of the other two.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(july_charter_sovereign_legitimacy__secular_democratic_reading, institutional, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
