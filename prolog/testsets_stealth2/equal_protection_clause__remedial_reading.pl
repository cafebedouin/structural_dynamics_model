% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection - Race-Conscious Remedial Mandate (Remedial Reading)
 *   domain: constitutional_law/political_philosophy/education_policy
 *
 * SUMMARY:
 *   This story instantiates the remedial reading of the equal-protection
 *   kernel: the claim that the Fourteenth Amendment requires government to
 *   employ race-conscious means to repair historically enforced group
 *   subordination until substantive equality is achieved. The standing
 *   arrangement under contest - and the referent of every metric here - is
 *   the race-conscious remediation apparatus as it actually operated in
 *   United States admissions, contracting, and employment from Bakke (1978)
 *   to SFFA (2023): recruitment pipelines, admissions preferences,
 *   procurement set-asides, and the compliance machinery that enforced them.
 *   The remedial reading affirms this arrangement as owed justice; consistent
 *   with the epsilon-referent rule, epsilon is nonetheless authored for the
 *   arrangement actual operation - the real, concentrated costs it imposed on
 *   identifiable non-preferred individuals - not for the achieved-equality
 *   counterfactual the reading endorses. The claim/metric independence rule
 *   is honored: claimed_type records the reading own structural
 *   self-understanding (a transitional arrangement carrying a declared sunset
 *   - remediation complete), while the metrics record what descriptively
 *   happened (rising extraction, hardening enforcement, and a justification
 *   migrating into performance as the sunset never acquired a trigger).
 *   Sibling readings (colorblind_reading, diversity_reading) are separate
 *   constraints authored separately; they appear here only as network links
 *   and committer omegas. KEY AGENTS (by structural relationship): -
 *   supreme_court_equal_protection_interpreter: agenda-setter
 *   (institutional/analytical) - administers the reading doctrinal
 *   boundaries; collects nothing, pays nothing -
 *   civil_rights_enforcement_agencies: agenda-setter
 *   (institutional/constrained) - builds and defends the enforcement
 *   machinery - selective_public_universities: administering implementer
 *   (institutional/constrained) - runs the programs, absorbs compliance and
 *   litigation costs, claims compositional gains - black_americans: primary
 *   beneficiary group (organized/trapped) - principal remedial claimant -
 *   native_american_communities, latino_communities: beneficiary groups
 *   (organized/trapped) - preferred_group_slot_recipients: direct receipt
 *   seat (moderate/constrained) - the individuals who occupy transferred
 *   positions - asian_american_applicants: primary payer
 *   (moderate/constrained) - largest per-capita admission penalty -
 *   white_applicants: payer (moderate/constrained) - diffuse costs; electoral
 *   base of the ban movement - non_minority_contracting_firms: payer with
 *   litigation leverage (powerful/mobile) - race_neutral_remedy_advocates:
 *   excluded voice (organized/constrained) - class-based and colorblind
 *   alternatives outside the operative framework -
 *   constitutional_law_scholars: analytical observer
 *   (analytical/civilizational)
 *
 * KEY AGENTS:
 *   - supreme_court_equal_protection_interpreter: agenda-setter (institutional/analytical) - administers the reading doctrinal boundaries; no material stake
 *   - civil_rights_enforcement_agencies: agenda-setter (institutional/constrained) - builds and defends the enforcement machinery
 *   - selective_public_universities: administering implementer (institutional/constrained) - runs the programs, bears compliance and litigation costs, claims compositional gains
 *   - black_americans: primary beneficiary group (organized/trapped) - principal remedial claimant
 *   - native_american_communities: beneficiary group (organized/trapped)
 *   - latino_communities: beneficiary group (organized/trapped)
 *   - preferred_group_slot_recipients: direct receipt seat (moderate/constrained) - individuals occupying transferred positions
 *   - asian_american_applicants: primary payer (moderate/constrained) - largest per-capita admission penalty
 *   - white_applicants: payer (moderate/constrained) - diffuse costs, ban-election base
 *   - non_minority_contracting_firms: payer with litigation leverage (powerful/mobile) - Croson/Adarand challengers
 *   - race_neutral_remedy_advocates: excluded voice (organized/constrained) - class-based and colorblind alternatives kept outside the operative framework
 *   - constitutional_law_scholars: analytical observer (analytical/civilizational)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.74).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.8).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection - Race-Conscious Remedial Mandate (Remedial Reading)").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "constitutional_law/political_philosophy/education_policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '25cc2093-d263-426a-a1a6-12f79895273d').
narrative_ontology:cs_kernel_codification('25cc2093-d263-426a-a1a6-12f79895273d', fixed_text).
narrative_ontology:cs_authority_grounding('25cc2093-d263-426a-a1a6-12f79895273d', lineage).
narrative_ontology:cs_interpretation_layer_present('25cc2093-d263-426a-a1a6-12f79895273d').
narrative_ontology:cs_reading_relation('25cc2093-d263-426a-a1a6-12f79895273d', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('25cc2093-d263-426a-a1a6-12f79895273d', equal_protection_clause__diversity_reading, influences).
narrative_ontology:cs_axiom('25cc2093-d263-426a-a1a6-12f79895273d', foundational, historical_subordination_creates_present_remedial_duty).
narrative_ontology:cs_axiom_status(historical_subordination_creates_present_remedial_duty, holdable).
narrative_ontology:cs_axiom_grounding('25cc2093-d263-426a-a1a6-12f79895273d', historical_subordination_creates_present_remedial_duty, deontological).
narrative_ontology:cs_axiom('25cc2093-d263-426a-a1a6-12f79895273d', secondary, formal_neutrality_perpetuates_subordination).
narrative_ontology:cs_axiom_status(formal_neutrality_perpetuates_subordination, holdable).
narrative_ontology:cs_axiom_grounding('25cc2093-d263-426a-a1a6-12f79895273d', formal_neutrality_perpetuates_subordination, empirically_contingent).
narrative_ontology:cs_reference_frame('25cc2093-d263-426a-a1a6-12f79895273d', substantive_group_equality_charter).
narrative_ontology:cs_drift_state('25cc2093-d263-426a-a1a6-12f79895273d', contemporary_post_sffa_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('25cc2093-d263-426a-a1a6-12f79895273d', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, black_americans).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, native_american_communities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, latino_communities).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, preferred_group_slot_recipients).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, asian_american_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, white_applicants).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, non_minority_contracting_firms).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, selective_public_universities).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, substantive_equality_doctrine).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, corrective_justice_principle).
narrative_ontology:constraint_vindicates(equal_protection_clause__remedial_reading, intergenerational_disparity_causation_thesis).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Fourteenth Amendment and decides which race-conscious programs survive. Under this reading it sustains programs backed by documented remedial findings and strikes those lacking them; its precedents (Fullilove, Croson, Adarand, Grutter, SFFA) define the mandate reach. It collects no material benefit and bears no programmatic cost; its exposure is doctrinal - each ruling reallocates authority among the rival readings of the same text.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, supreme_court_equal_protection_interpreter, agenda_setter,
    institutional, generational, analytical, national).

% Federal civil-rights bodies press institutions to adopt race-conscious remedies through compliance reviews, consent agreements, and funding conditions, and defend the mandate legality in court. Their budgets and missions expand with the mandate scope; abandoning enforcement would contradict their statutory purpose.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, civil_rights_enforcement_agencies, agenda_setter,
    institutional, generational, constrained, national).

% Administer race-conscious admissions under the mandate: set targets, weigh race in file review, report compliance, and absorb litigation costs, protest disruption, and the administrative load of facially neutral proxy engineering. They also claim educational gains from the resulting composition. Leaving the mandate entirely would invite federal enforcement action and funding loss; staying exposes them to suits from rejected applicants.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, selective_public_universities, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(equal_protection_clause__remedial_reading, selective_public_universities, payer).

% The principal group the remediation addresses: descendants of enslaved and Jim Crow-subjected Americans whose wealth, schooling, and health statistics remain sharply below the national median. Members receive dedicated recruitment, admissions consideration, and program set-asides; the group claim on the mandate is grounded in documented state-enforced subordination. There is no alternative venue where this historical claim can be collected.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, black_americans, beneficiary,
    organized, generational, trapped, national).

% Treaty violations, dispossession, and boarding-school assimilation left sovereign tribal nations with the country worst poverty and health indices. Members receive admissions consideration and contracting preferences; tribal governments additionally litigate to keep the mandate protections from being struck as racial classifications rather than political relationships with sovereigns.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, native_american_communities, beneficiary,
    organized, generational, trapped, national).

% Communities shaped by conquest, annexation, and labor-recruitment histories, with large young populations and below-median attainment. Members receive recruitment, admissions consideration, and set-aside eligibility; community organizations defend the mandate continuation in court and at the ballot.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, latino_communities, beneficiary,
    organized, generational, trapped, national).

% The individual applicants, hires, and bidders who actually occupy the positions the mandate transfers: the admitted student, the awarded contractor, the promoted manager. Each receives a concrete opportunity he or she would plausibly not have received under race-neutral allocation; none chose the mechanism, and each carries the public debate that attaches to preference-held positions.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, preferred_group_slot_recipients, beneficiary,
    moderate, biographical, constrained, national).

% Applicants from a group that is simultaneously historically excluded (Chinese Exclusion, internment) and statistically overrepresented at selective institutions, so that race-conscious caps bind hardest against them. They bear the largest per-capita admission-probability penalty under the mandate; their families respond with test-score escalation, and since 2014 they have organized litigation challenging the classifications.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, asian_american_applicants, payer,
    moderate, biographical, constrained, national).

% Applicants and workers outside every preferred category. They bear diffuse per-capita costs - somewhat lower admission and hiring probabilities - and supply much of the electoral base for statewide bans. Individually they rarely litigate; collectively they fund initiative campaigns and legal-defense organizations.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, white_applicants, payer,
    moderate, biographical, constrained, national).

% Construction and supply firms ineligible for race-conscious set-asides on public work. They bid on the remainder of each contract, form trade associations, and brought the Croson and Adarand challenges that rewrote the mandate evidentiary requirements. Their exit is comparatively easy: private-sector work and non-preference jurisdictions absorb them.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, non_minority_contracting_firms, payer,
    powerful, biographical, mobile, national).

% Class-based-preference proponents, place-based investment advocates, and colorblind jurists who argue remediation should track disadvantage directly or reject racial sorting altogether. During the mandate ascendancy they spoke from dissents, state ballot campaigns, and minority think-tank reports rather than from enforcement tables; their proposals were procedurally outmatched wherever the mandate governed.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, race_neutral_remedy_advocates, excluded,
    organized, biographical, constrained, national).

% Academic interpreters who map the amendment competing readings, publish genealogies of the remedial tradition from the Reconstruction amendments through SFFA, and supply the doctrinal vocabulary both litigating sides borrow. They bear none of the allocation costs and receive none of its transfers.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, constitutional_law_scholars, observer,
    analytical, civilizational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_clause__remedial_reading, preferred_group_slot_recipients).
narrative_ontology:fixing_cost_class(equal_protection_clause__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a collective-action problem no individual actor or race-neutral rule solves unilaterally: converting documented, intergenerationally transmitted group disadvantage into present access by coordinating opportunity allocation across thousands of institutions simultaneously, so that remedial burden is shared and cumulative rather than heroic and episodic.
% TRANSFER_FUNCTION: Moves admissions places, contracts, hiring slots, and the lifetime earnings and status attached to them from individual members of non-preferred racial groups to members of historically subordinated groups, mediated by institutional classification and enforced by courts and compliance agencies.
% ABSENT_VOICES: Individual non-preferred applicants at the moment of decision - their costs aggregate into statistics and no seat represents them as a class; class-based and place-based remedy advocates, whose proposals were procedurally outmatched wherever the mandate governed; and colorblind jurists, confined during the interval to dissents and state ballot campaigns. Their absence mattered: unanimity in elite venues reflected the framework boundaries, not the assent of everyone affected.
% DISAPPEARANCE_RATIONALE: Overnight removal redistributes admissions and contract awards immediately toward race-neutral criteria, terminates dedicated pipelines and set-asides, and leaves the underlying group gaps in place by default; beneficiary-group institutions lose their principal conversion channel for historical claims, and enforcement agencies lose their core docket. The world does not stay put - it reorganizes around the colorblind default the sibling reading describes.
% FOUNDING_PROBLEM: State-enforced racial subordination - slavery, Jim Crow, dispossession, exclusion acts - produced durable group disparities in wealth, education, and political power that formally neutral rules transmit rather than correct.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Federal Reserve and Census wealth-gap series document persistent disparity; the SFFA majority opinion itself concedes the historical record of discrimination while disputing the remedy; intergenerational-mobility research ties specific historical policy exposures to present outcomes. No corroborating source attests that the founding problem is solved; the contest is over whether this instrument addresses it, not over whether the problem existed.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.74) reflects the arrangement actual operation at interval end: opportunity transfers concentrated on identifiable individuals (admission denials, lost bids), large relative to the stakes of the decisions, sustained across forty-five years and successive cohorts. Suppression (0.80) is authored as a raw structural property - it is not scaled by power or scope; only extractiveness is scaled by the engine. Suppression is high because the mandate ran on legal compulsion: institutions faced enforcement action for non-adoption, individuals had no opt-out from racial classification, and dissenting jurisdictions escaped only by constitutional amendment (Proposition 209 and successors). Theater (0.72) tracks the migration of justification: after Croson and Adarand raised the evidentiary price of remedial findings, and after Grutter elevated diversity, the remedial rationale increasingly operated as recited cover while the underlying mechanisms persisted - by 2023 much maintenance activity defended the categories rather than performed remediation. Accessibility collapse (0.58) is moderate: race-neutral substitutes existed and were tried (percent plans, class preferences), but the reading itself holds them insufficient, and ban-state exits required extraordinary political expenditure. Resistance (0.78) is among the highest recorded for a modern constitutional arrangement: sustained litigation, five statewide bans, persistent polling majorities opposed, payer-side coalition formation (trade associations bringing Croson and Adarand; the Asian American legal coalition behind SFFA), and finally controlling-precedent reversal. The three measurement series share one time grid (1978-2023, eight points) so every metric is authored at every examined point; trajectories are monotonic rather than cyclical - no intermittent-reinforcement mechanism is alleged. suppression_requirement is tracked rather than left static because the enforcement picture genuinely changed: compliance pressure, then litigation defense, then workaround engineering - a hardening ratchet, not a stable baseline.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the beneficiary-group seats the arrangement is a debt arriving: the only channel through which documented state wrongdoing converts into present opportunity, with no alternative collection mechanism - exit is trapped because the claim is on history, not on a market. From the payer seats the identical structure is race-assigned denial: an applicant experiences it as a probability of rejection assigned by ancestry, with exit merely the choice of which similarly configured competition to enter. From the administering implementers it is a compliance environment: litigation and proxy-engineering costs weighed against claimed compositional benefits. From the agenda-setting court it is boundary administration among rival readings of one text. From the excluded advocates it is a category error - remediation aimed at ancestry rather than disadvantage. The engine derives these divergences from role, power, and exit data; the divergence between the claimed transitional type and the extraction-heavy computed profile at the payer seats is the measurement this story contributes.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (black_americans, native_american_communities, latino_communities, preferred_group_slot_recipients) drive d toward the beneficiary pole for those seats; the trap-shaped exit (no alternative channel for historical redress) anchors them near full subsidy. Victim declarations (asian_american_applicants, white_applicants, non_minority_contracting_firms) drive d toward the target pole; constrained exit for applicants amplifies effective extraction, while the firms mobile exit damps theirs - the same nominal payer seat sits at different effective extraction by exit class. Administering implementers (selective_public_universities) are dual-positioned: agenda_setter with a payer secondary role, so their derived d sits mid-range rather than at the beneficiary pole. Courts and enforcement agencies hold administrative seats with analytical or constrained exit and no material receipts, deriving near-symmetric. No directionality_overrides are authored: the override lever is keyed to power atoms, and this story institutional seats (court, agencies, universities) share the institutional atom while holding genuinely different relationships - an override would cross-contaminate them. The role-and-exit derivation is finer-grained than the override available.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - durable group disparity produced by state-enforced subordination - is corroborated by sources outside the benefiting parties (federal wealth-gap statistics, the SFFA majority own concession of historical discrimination), so the problem is not dead; but whether THIS instrument still addresses it is disputed, hence founding_problem_status: contested paired with disappearance_verdict: world_rearranges. That pairing is the mandatrophy-relevant signal: the arrangement sunset (remediation complete) never acquired a determinate trigger, so the transitional self-understanding could not fire, and the interval records the consequence - theater_ratio climbing past 0.5 as the justification migrated to diversity while the mechanisms persisted. The classification prevents symmetric mislabeling: calling the arrangement a pure coordination device would erase the identifiable payers its operation requires; calling it pure extraction would erase the genuine remedial transfer that constituted its function and its moral warrant. The scaffold claim preserves the reading own transitional logic while the authored metrics expose the unfiring sunset - leaving the engine to measure whether a declared sunset without a trigger is a transitional arrangement or an inertia-maintained one. The accumulation signature (rising base_extractiveness across the interval) is present in the measurement series and stands as the hypothesis a drift investigation would test.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completion_criterion,
    'What observable state counts as remediation complete - outcome parity, opportunity parity, per-cohort parity, or elapsed time - and which institution is authorized to declare it?',
    'Legislative or judicial adoption of a determinate completion metric with a designated declaring authority; alternatively, sustained bipartisan specification of sunset conditions in reauthorization statutes.',
    'With a determinate trigger the arrangement operates as a genuine transitional support whose sunset can fire; without one the sunset is declaratory only and the arrangement persists past its stated function, degrading toward inertia-maintained operation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remediation_completion_criterion, conceptual, 'Indeterminate sunset trigger for the remedial mandate.').

omega_variable(
    group_vs_individual_rights_unit,
    'Does the Fourteenth Amendment guarantee run to racial groups (owed remediation as such) or to individuals (owed non-subordination), such that group-targeted classification is a fulfillment or a violation?',
    'Not resolvable by data alone; turns on jurisprudential commitment forged in constitutional moments (ratification debates, Reconstruction amendments, controlling precedent). Track whether a future amendment or supermajority consensus re-specifies the unit.',
    'If the individual is the unit, the entire beneficiary/victim structure inverts and this reading collapses into its colorblind sibling; if the group is the unit, the mandate structure stands.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(group_vs_individual_rights_unit, conceptual, 'The rights-bearing-unit axis separating this reading from the colorblind sibling.').

omega_variable(
    causal_legacy_share,
    'What share of present measured group disparity is causally attributable to historical state-enforced subordination rather than to post-1970 demographic, cultural, or selection factors?',
    'Quasi-experimental and intergenerational-mobility research linking specific historical policy exposures (redlining maps, segregation intensity) to present outcomes; natural experiments from policy discontinuities.',
    'A falling legacy share erodes the mandate warrant and pushes the classification toward unjustified burden; a robust legacy share sustains the remedial justification and the reading coherence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causal_legacy_share, empirical, 'Empirical strength of the historical-causation warrant.').

omega_variable(
    race_neutral_substitutability,
    'Can race-neutral instruments (percent plans, class-based preferences, place-based investment) deliver comparable remediation of group disparity, or is race-consciousness causally necessary?',
    'Compare outcome trajectories in ban states (California, Michigan, Florida percent-plan regimes) against matched race-conscious jurisdictions on enrollment, completion, and wealth convergence.',
    'High substitutability converts the mandate necessity claim into a mere preference claim and raises its effective burden; low substitutability confirms the coordination function the mandate performs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(race_neutral_substitutability, empirical, 'Whether the mandate instrument is necessary or substitutable.').

omega_variable(
    sibling_displacement_trajectory,
    'Is the colorblind reading 2023 ascendance terminal for this reading, or will revival pressure restore remedial variants (slavery-descendant-specific programs, tribal-political classifications, state-level equivalents)?',
    'Track post-2023 case law, state legislation, and institutional adoption of ancestry-specific instruments that evade strict scrutiny.',
    'Terminal displacement closes this constraint interval; successful revival restarts the accumulation cycle with narrower beneficiary sets.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sibling_displacement_trajectory, empirical, 'Whether the reading constraint is closing or cycling.').

omega_variable(
    kernel_reading_instantiation,
    'This constraint is one reading of the equal_protection_clause kernel - what structurally changes if a sibling reading (colorblind_reading, diversity_reading) governs instead, and where exactly do the readings disagree?',
    'Comparative classification of the sibling stories: colorblind deletes the beneficiary/victim structure outright (all racial classification forbidden); diversity replaces group remediation with diffuse educational benefit (beneficiary set becomes all students, victim set dissolves into tradeoffs). The disagreement is located in the rights-bearing unit and in whether historical subordination generates present remedial duties.',
    'Classification of this constraint is valid only for the remedial reading; importing sibling premises would change epsilon, the beneficiary/victim sets, and the computed type wholesale.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_instantiation, conceptual, 'Committer-frame routing: one reading of a three-reading kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ep_remedial_tr_t1978, equal_protection_clause__remedial_reading, theater_ratio, 1978, 0.22).
narrative_ontology:measurement_basis(ep_remedial_tr_t1978, observed).
narrative_ontology:measurement(ep_remedial_tr_t1984, equal_protection_clause__remedial_reading, theater_ratio, 1984, 0.3).
narrative_ontology:measurement_basis(ep_remedial_tr_t1984, observed).
narrative_ontology:measurement(ep_remedial_tr_t1990, equal_protection_clause__remedial_reading, theater_ratio, 1990, 0.4).
narrative_ontology:measurement_basis(ep_remedial_tr_t1990, observed).
narrative_ontology:measurement(ep_remedial_tr_t1996, equal_protection_clause__remedial_reading, theater_ratio, 1996, 0.5).
narrative_ontology:measurement_basis(ep_remedial_tr_t1996, observed).
narrative_ontology:measurement(ep_remedial_tr_t2003, equal_protection_clause__remedial_reading, theater_ratio, 2003, 0.58).
narrative_ontology:measurement_basis(ep_remedial_tr_t2003, observed).
narrative_ontology:measurement(ep_remedial_tr_t2010, equal_protection_clause__remedial_reading, theater_ratio, 2010, 0.65).
narrative_ontology:measurement_basis(ep_remedial_tr_t2010, observed).
narrative_ontology:measurement(ep_remedial_tr_t2016, equal_protection_clause__remedial_reading, theater_ratio, 2016, 0.7).
narrative_ontology:measurement_basis(ep_remedial_tr_t2016, observed).
narrative_ontology:measurement(ep_remedial_tr_t2023, equal_protection_clause__remedial_reading, theater_ratio, 2023, 0.72).
narrative_ontology:measurement_basis(ep_remedial_tr_t2023, observed).

% Extraction over time
narrative_ontology:measurement(ep_remedial_be_t1978, equal_protection_clause__remedial_reading, base_extractiveness, 1978, 0.54).
narrative_ontology:measurement_basis(ep_remedial_be_t1978, observed).
narrative_ontology:measurement(ep_remedial_be_t1984, equal_protection_clause__remedial_reading, base_extractiveness, 1984, 0.59).
narrative_ontology:measurement_basis(ep_remedial_be_t1984, observed).
narrative_ontology:measurement(ep_remedial_be_t1990, equal_protection_clause__remedial_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement_basis(ep_remedial_be_t1990, observed).
narrative_ontology:measurement(ep_remedial_be_t1996, equal_protection_clause__remedial_reading, base_extractiveness, 1996, 0.69).
narrative_ontology:measurement_basis(ep_remedial_be_t1996, observed).
narrative_ontology:measurement(ep_remedial_be_t2003, equal_protection_clause__remedial_reading, base_extractiveness, 2003, 0.71).
narrative_ontology:measurement_basis(ep_remedial_be_t2003, observed).
narrative_ontology:measurement(ep_remedial_be_t2010, equal_protection_clause__remedial_reading, base_extractiveness, 2010, 0.73).
narrative_ontology:measurement_basis(ep_remedial_be_t2010, observed).
narrative_ontology:measurement(ep_remedial_be_t2016, equal_protection_clause__remedial_reading, base_extractiveness, 2016, 0.74).
narrative_ontology:measurement_basis(ep_remedial_be_t2016, observed).
narrative_ontology:measurement(ep_remedial_be_t2023, equal_protection_clause__remedial_reading, base_extractiveness, 2023, 0.74).
narrative_ontology:measurement_basis(ep_remedial_be_t2023, observed).

% Suppression requirement over time
narrative_ontology:measurement(ep_remedial_su_t1978, equal_protection_clause__remedial_reading, suppression_requirement, 1978, 0.46).
narrative_ontology:measurement_basis(ep_remedial_su_t1978, observed).
narrative_ontology:measurement(ep_remedial_su_t1984, equal_protection_clause__remedial_reading, suppression_requirement, 1984, 0.53).
narrative_ontology:measurement_basis(ep_remedial_su_t1984, observed).
narrative_ontology:measurement(ep_remedial_su_t1990, equal_protection_clause__remedial_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement_basis(ep_remedial_su_t1990, observed).
narrative_ontology:measurement(ep_remedial_su_t1996, equal_protection_clause__remedial_reading, suppression_requirement, 1996, 0.67).
narrative_ontology:measurement_basis(ep_remedial_su_t1996, observed).
narrative_ontology:measurement(ep_remedial_su_t2003, equal_protection_clause__remedial_reading, suppression_requirement, 2003, 0.71).
narrative_ontology:measurement_basis(ep_remedial_su_t2003, observed).
narrative_ontology:measurement(ep_remedial_su_t2010, equal_protection_clause__remedial_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement_basis(ep_remedial_su_t2010, observed).
narrative_ontology:measurement(ep_remedial_su_t2016, equal_protection_clause__remedial_reading, suppression_requirement, 2016, 0.78).
narrative_ontology:measurement_basis(ep_remedial_su_t2016, observed).
narrative_ontology:measurement(ep_remedial_su_t2023, equal_protection_clause__remedial_reading, suppression_requirement, 2023, 0.8).
narrative_ontology:measurement_basis(ep_remedial_su_t2023, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, resource_allocation).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% Colloquial equal protection conflates three structurally distinct constraints sharing one text-kernel: forbid-all-classifications (colorblind_reading), permit-for-diversity (diversity_reading), require-for-remediation (this file). Their epsilon values diverge widely because their beneficiary/victim structures differ: the colorblind instantiation has no remedial beneficiary set; the diversity instantiation spreads benefits across all students; this instantiation concentrates benefits on historically subordinated groups and costs on non-preferred individuals. The remedial reading is upstream of the diversity reading historically (its doctrinal constriction after Croson/Adarand created the legitimacy space diversity filled) and logically incompatible with the colorbound reading. Each reading is authored as a separate epsilon-invariant story; this file links both siblings via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
