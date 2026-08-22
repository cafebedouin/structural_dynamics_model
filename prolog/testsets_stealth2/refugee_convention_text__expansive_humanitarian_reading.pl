% ============================================================================
% CONSTRAINT STORY: refugee_convention_text__expansive_humanitarian_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
 *   human_readable: Refugee Convention — Expansive Humanitarian Reading (Broad Protection Mandate)
 *   domain: international_law/migration_governance/human_rights
 *
 * SUMMARY:
 *   This story instantiates ONE reading of a contested kernel: the 1951
 *   Convention Relating to the Status of Refugees and its 1967 Protocol, read
 *   as an unbendable humanitarian mandate. On this reading the text's
 *   operative content is broad: 'well-founded fear' reaches generalized
 *   violence and persecution by non-state actors; 'particular social group'
 *   reaches gender, sexual orientation, and clan membership; the non-return
 *   obligation follows the person to the high seas and to offshore processing
 *   facilities; and every claimant is entitled to substantive individual
 *   assessment. The parties are states (paying), displaced persons
 *   (protected), and the protection apparatus (administering and expanding
 *   the mandate). The epsilon referent is the standing arrangement under
 *   contest — the Convention-as-operative-protection-regime — assessed by
 *   this reading's own lights: the reading sees the regime's transfers as
 *   duties owed rather than rents taken, and authors epsilon accordingly.
 *   Sibling readings (restrictive_sovereignty_reading,
 *   procedural_integrity_reading) are separate constraint files linked
 *   through network.affects_constraints; their structural deltas are recorded
 *   in omega variables, not in this constraint's body.
 *
 * KEY AGENTS:
 *   - asylum_seekers_refugees: protected class (powerless/trapped) — receive substantive assessment and non-return protection; the mandate's point and its principal recipients
 *   - unhcr_protection_mandate: agenda-setter and institutional beneficiary (institutional/identity_locked) — supervises, interprets, and expands the mandate; budget and relevance track its breadth
 *   - refugee_status_determination_judiciaries: co-agenda-setter (institutional/constrained) — converts guideline language into binding holdings case by case
 *   - refugee_rights_ngo_sector: beneficiary (organized/mobile) — litigates and monitors; funding, standing, and careers ride on breadth
 *   - frontline_host_states: primary payers (powerful/trapped) — geography-fixed exposure against voluntary, late, small burden-sharing
 *   - destination_state_governments: payers (institutional/constrained) — fund the systems, attempt externalization, lose devices to litigation
 *   - border_enforcement_agencies: payers (institutional/constrained) — interception toolkit successively ruled out, adapts and relitigates
 *   - host_district_communities: diffuse local payers (moderate/trapped) — service strain and friction policing with little voice
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(refugee_convention_text__expansive_humanitarian_reading, 0.44).
domain_priors:suppression_score(refugee_convention_text__expansive_humanitarian_reading, 0.6).
domain_priors:theater_ratio(refugee_convention_text__expansive_humanitarian_reading, 0.32).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, extractiveness, 0.44).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0.32).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, accessibility_collapse, 0.28).
narrative_ontology:constraint_metric(refugee_convention_text__expansive_humanitarian_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(refugee_convention_text__expansive_humanitarian_reading, tangled_rope).
narrative_ontology:human_readable(refugee_convention_text__expansive_humanitarian_reading, "Refugee Convention — Expansive Humanitarian Reading (Broad Protection Mandate)").
narrative_ontology:topic_domain(refugee_convention_text__expansive_humanitarian_reading, "international_law/migration_governance/human_rights").

domain_priors:requires_active_enforcement(refugee_convention_text__expansive_humanitarian_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(refugee_convention_text__expansive_humanitarian_reading, '0fbe61d3-d5e4-4096-a03f-61a0c5edee91').
narrative_ontology:cs_kernel_codification('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', fixed_text).
narrative_ontology:cs_authority_grounding('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', distributed).
narrative_ontology:cs_reading_relation('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', refugee_convention_text__restrictive_sovereignty_reading, coexists_with).
narrative_ontology:cs_reading_relation('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', refugee_convention_text__procedural_integrity_reading, influences).
narrative_ontology:cs_axiom('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', foundational, non_return_follows_the_person_everywhere).
narrative_ontology:cs_axiom_status(non_return_follows_the_person_everywhere, holdable).
narrative_ontology:cs_axiom_grounding('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', non_return_follows_the_person_everywhere, deontological).
narrative_ontology:cs_axiom('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', foundational, social_group_ground_reaches_gender_orientation_clan).
narrative_ontology:cs_axiom_status(social_group_ground_reaches_gender_orientation_clan, holdable).
narrative_ontology:cs_axiom_grounding('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', social_group_ground_reaches_gender_orientation_clan, deontological).
narrative_ontology:cs_reference_frame('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', unbendable_humanitarian_protection_mandate).
narrative_ontology:cs_drift_state('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', contemporary_externalization_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('0fbe61d3-d5e4-4096-a03f-61a0c5edee91', '').
narrative_ontology:cs_kernel_id(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_refugees).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate).
narrative_ontology:constraint_beneficiary(refugee_convention_text__expansive_humanitarian_reading, refugee_rights_ngo_sector).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, frontline_host_states).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, destination_state_governments).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, border_enforcement_agencies).
narrative_ontology:constraint_victim(refugee_convention_text__expansive_humanitarian_reading, host_district_communities).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, non_refoulement_customary_status).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, cartagena_expansive_definition).
narrative_ontology:constraint_vindicates(refugee_convention_text__expansive_humanitarian_reading, gender_related_persecution_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Flee generalized violence, non-state persecution, or gender-, sexual-orientation-, or clan-based targeting. Everything the mandate provides — substantive assessment of the claim, a hearing before any removal, protection from being sent back — arrives only if a state processes the case on the merits. They cannot return home, cannot choose their destination (visa and carrier regimes decide that), and do not appear as parties in the treaty bodies and negotiations that shape the rules; they act through counsel and advocates or not at all.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_refugees, beneficiary,
    powerless, immediate, trapped, global).

% Supervises the Convention, issues interpretive guidelines (gender, exclusion clauses, emergency standards), intervenes in litigation, and convenes the pledging and compact processes through which the mandate's content is elaborated. Its budget grew from under a billion dollars annually in the early 1980s to over ten billion; its institutional relevance tracks the breadth of the protection obligation. It depends on voluntary state contributions it does not control, and it cannot abandon the supervisory role without dissolving its own reason for existence.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate, agenda_setter,
    institutional, generational, identity_locked, global).
narrative_ontology:stakeholder_secondary_role(refugee_convention_text__expansive_humanitarian_reading, unhcr_protection_mandate, beneficiary).

% Litigates test cases that widen the protected categories, staffs legal aid for claimants, monitors pushbacks, and publishes the compliance reports states must answer. Funding, standing, and professional careers concentrate around the breadth of the obligation; individual organizations can shift portfolios if a legal front closes, but the sector as a whole exists because the mandate generates cases to fight.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugee_rights_ngo_sector, beneficiary,
    organized, biographical, mobile, global).

% States bordering conflict zones — Turkey, Jordan, Lebanon, Pakistan, Kenya, Colombia at various periods — receive the largest arrivals regardless of capacity or consent. They finance camps, services, and security for populations they did not choose, while burden-sharing pledges arrive late, small, and voluntary. Geography fixes their exposure; denouncing the Convention would cost more than compliance, so they comply while lobbying for others to share the load.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, frontline_host_states, payer,
    powerful, generational, trapped, regional).

% Fund asylum adjudication systems, reception, welfare, and integration, and answer electorally for arrival numbers. They attempt to reduce exposure through visa regimes, carrier sanctions, safe-third-country rules, externalized processing, and interdiction; each device draws litigation under the mandate's non-return rule, and courts have repeatedly extended protection to interceptions at sea and to offshore facilities. Withdrawal from the treaty system is available on paper and politically unusable.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, destination_state_governments, payer,
    institutional, biographical, constrained, national).

% Operate interception, pushback, and removal machinery whose core tools — turning boats around, chain removals through transit states, offshore detention — have been successively ruled incompatible with the non-return obligation. Operational discretion narrows with each ruling; the agencies adapt with new devices, which restarts the cycle.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, border_enforcement_agencies, payer,
    institutional, biographical, constrained, national).

% Towns and districts adjacent to camps and reception centers absorb housing, school, and clinic strain and police the resulting frictions, with little voice in national bargaining over arrivals and little access to the compensation funds pledged at international conferences. Some local economies gain customers and labor; the net position varies block by block and is rarely measured.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, host_district_communities, payer,
    moderate, immediate, trapped, local).

% National asylum courts and regional human rights courts convert guideline language into binding holdings — recognizing particular-social-group claims, extending non-return to maritime interception, requiring individual examination before any removal. They are bound by precedent and legal method, cannot decline the docket, and their caseload grows with every widening of the protected categories.
narrative_ontology:constraint_stakeholder(refugee_convention_text__expansive_humanitarian_reading, refugee_status_determination_judiciaries, agenda_setter,
    institutional, generational, constrained, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(refugee_convention_text__expansive_humanitarian_reading, asylum_seekers_refugees).
narrative_ontology:fixing_cost_class(refugee_convention_text__expansive_humanitarian_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves a genuine collective-action problem: no single state will protect the persecuted at scale alone, because protection is costly and its benefits — stability, legality, reciprocity — are diffuse. A common definition of who qualifies, a shared non-return floor, and supervised interpretation prevent a race to the bottom in which each state's laxity exports danger to its neighbors, and give displaced persons a predictable path to safety somewhere.
% TRANSFER_FUNCTION: Moves money, territory, and adjudication: fiscal resources from state treasuries to reception, welfare, and protection programming; territorial admission from destination and frontline states to displaced persons; decision-making effort from state bureaucracies to courts and status-determination systems; and interpretive authority to UNHCR and the judiciary.
% ABSENT_VOICES: Refugees themselves almost never sit as parties: claims are decided about them in proceedings they attend as applicants, and the treaties and compacts that shape the rules are negotiated by states with UNHCR at the table but no elected refugee representation. Displaced people who die en route or never reach a border are outside the mandate's reach entirely and absent from every forum. Origin-country communities whose destabilization produces the flows have no seat either.
% DISAPPEARANCE_RATIONALE: If the mandate vanished overnight, interception and return would become the default at every frontier, protection would shrink to whatever bilateral bargains destination states found convenient, UNHCR's supervisory leverage and the litigation architecture would lose their legal basis, and the millions of claims now decided on the merits would be decided by executive discretion. The protection economy — camps, adjudication, resettlement, the legal sector — would reorganize around charity and diplomacy instead of entitlement.
% FOUNDING_PROBLEM: Mass statelessness and displacement after the Second World War, and the documented refusal of states to admit Jewish and other persecuted refugees fleeing Nazi persecution, which the drafters treated as a civilizational failure requiring a binding, non-discretionary guarantee that no person fleeing persecution be returned to harm.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: conflict-event datasets (UCDP, ACLED) and investigative journalism document the wars and persecutions driving displacement at record scale; origin-country civil society and bar associations document the targeted violence behind individual claims; and destination-state intelligence and development agencies treat displacement volumes as operational facts in their own planning. None of these sources depends on the protection apparatus for standing.
narrative_ontology:disappearance_verdict(refugee_convention_text__expansive_humanitarian_reading, world_rearranges).
narrative_ontology:founding_problem_status(refugee_convention_text__expansive_humanitarian_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(refugee_convention_text__expansive_humanitarian_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(refugee_convention_text__expansive_humanitarian_reading, 'none', 1).
narrative_ontology:epsilon_provenance(refugee_convention_text__expansive_humanitarian_reading, 0.44, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is authored at 0.44: the mandate moves real, asymmetric costs onto states — concentrated geographically on frontline hosts, amplified by every widening of the protected categories — while this reading's own lights treat the transfer as legitimate duty; the scalar measures the transfer's magnitude, not its justification. Suppression (0.60) is the coercive force holding the arrangement against state preference: treaty entrenchment, litigation, and treaty-body pressure close off the devices states prefer, though visa regimes, carrier sanctions, and funding leverage remain open, so alternatives are narrowed rather than eliminated. Theater (0.32) sits mostly on the state side of the ledger — pledge summits, compact reviews, pro-forma compliance declarations — while the mandate's own machinery (hearings, rulings, registration) is functional. Accessibility_collapse is low (0.28): understanding the expansive reading does not exhaust the space of readings; the restrictive and procedural alternatives remain live and practiced. Resistance is high (0.75): interdiction fleets, offshore processing, externalization treaties, pushbacks, and defunding campaigns are the arrangement's daily opposition. All three temporal series run on one shared nine-point grid. The base_extractiveness series oscillates with conflict cycles (Balkans, Syria, Ukraine surges) superimposed on a slow rise — the oscillation is driven by external war frequency, not by intermittent reinforcement engineered into the arrangement, and the base_properties scalars reflect the interval-end state. The suppression_requirement series is authored deliberately: this story traces enforcement-capacity change, from exhortatory supervision in the early 1980s to binding judicial enforcement, so the rising trajectory models machinery maturing and hardening, not extraction shifting. Suppression is authored as a raw structural property and left unscaled; only extractiveness is scaled by directionality and scope downstream. Coalition note: the payer seats are numerous but poorly coordinated — frontline states have repeatedly demanded binding burden-sharing and repeatedly settled for voluntary pledges, which is why the geographic asymmetry persists.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently by construction. From the refugee seat the arrangement is a lifeline whose failures are under-enforcement; from the frontline-state seat it is an unfunded imposition whose failures are over-demand; from UNHCR's seat it is the institution's constitutive purpose; from the judiciary's seat it is a doctrinal project advancing holding by holding; from the NGO seat it is a practice-generating machine. Payer seats and beneficiary/agenda-setter seats therefore experience different types from identical structural data — the engine computes that divergence; nothing in the authored claim adjudicates it. UNHCR's exit is authored identity_locked rather than merely constrained: the organization has become its mandate — supervision, interpretation, and expansion are not tasks it performs but what it is — so exit would require institutional self-dissolution rather than portfolio change; if that identity frame broke (for instance, if protection were ever reframed as a completed project), the seat's classification would shift sharply.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations (asylum_seekers_refugees, unhcr_protection_mandate, refugee_rights_ngo_sector) drive d toward the subsidized end; victim declarations (frontline_host_states, destination_state_governments, border_enforcement_agencies, host_district_communities) drive d toward the target end, amplified by trapped and constrained exits and damped nowhere by arbitrage-grade mobility. The derivation chain suffices for every seat, so no directionality_overrides are authored: the one same-power-atom ambiguity — institutional seats spanning UNHCR, destination governments, border agencies, and judiciaries — resolves cleanly through role and exit differences rather than needing correction.
 *
 * MANDATROPHY ANALYSIS:
 *   No mandatrophy: the founding problem — persecution and mass displacement — is live at record scale, corroborated by sources outside the beneficiary set, so the (status=live x verdict=world_rearranges) cell raises no zombie flag. The classification discipline matters in both directions: the genuine coordination function (common definition, non-return floor, supervised interpretation preventing a protection race-to-the-bottom) blocks a pure-extraction reading, while the asymmetric geographic burden, the actively enforced closure of state evasion devices, and the identifiable payer seats block a pure-coordination reading. The hybrid is the honest center. The residual risks run both ways: if burden-sharing were ever made binding and automatic, the cost asymmetry would compress toward coordination overhead and the structure would migrate toward rope; if enforcement decayed while state evasion devices multiplied, it would drift toward piton — doctrine intact, practice hollow.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This constraint is one reading of the refugee_convention_text kernel: do Articles 1A(2) and 33 fix a narrow, individualized, territorially-limited protection threshold (the restrictive sibling) or admit generalized violence, non-state persecution, broad social groups, and extraterritorial non-return (this reading)?',
    'Doctrinal trajectory tracking: cumulative holdings of high courts and regional human rights bodies on social-group breadth, pattern evidence, and extraterritorial application, read against state reservation and denunciation behavior.',
    'If the restrictive sibling prevails, the victim set contracts to individually provable, state-agent persecution; interdiction and offshore processing cease to be violations; this constraint''s epsilon and enforcement surface collapse toward the sibling''s profile.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the Convention text governs — the committer-frame contest itself.').

omega_variable(
    burden_allocation_by_geography,
    'Is the concentration of protection costs on first-arrival states a defect of this constraint, or of the missing complementary instrument (binding burden-sharing), given that the mandate itself is silent on allocation?',
    'Counterfactual comparison against jurisdictions that piloted binding allocation (EU Dublin reform proposals, quota mechanisms): measure whether frontline-state effective burden converges toward capacity when allocation is binding.',
    'If allocation is the defect, the constraint''s costs concentrate severely on trapped frontline seats — their effective burden far exceeds the aggregate figure — and the structure leans toward pure extraction for those seats; if complementary instruments can fix it, the asymmetry is transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(burden_allocation_by_geography, empirical, 'Whether geographic cost concentration is intrinsic to the mandate or repairable by companion instruments.').

omega_variable(
    psg_outer_boundary,
    'Where does ''particular social group'' stop? Gender, sexual orientation, and clan are settled extensions of this reading; profession, economic class, and politically adjacent categories remain contested — does the ground expand until it collapses into the other four persecution grounds?',
    'Comparative case-law cluster analysis across contracting states: map which social-group formulations survive appellate review and which are absorbed into opinion, religion, or race grounds.',
    'Each further extension widens the victim set and raises the mandated enforcement load; a judicially drawn outer boundary stabilizes the reading''s scope and its cost profile.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(psg_outer_boundary, conceptual, 'Indeterminacy internal to this reading''s central definitional extension.').

omega_variable(
    extraterritorial_refoulement_doctrine,
    'Does the reading''s extension of non-return to maritime interdiction and offshore processing rest on settled extraterritorial-jurisdiction doctrine, or on an interpretive stretch that continuing state practice and some courts still reject?',
    'Jurisprudential consolidation test: whether apex and regional courts converge on control-based jurisdiction for interception and offshore facilities, and whether state practice abandons the devices or relitigates them indefinitely.',
    'If consolidated, the mandate binds the states'' principal evasion tools and the enforcement surface is stable; if not, the reading''s most operationally important extension stays permanently contested and effective protection varies by sea lane.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraterritorial_refoulement_doctrine, empirical, 'Doctrinal solidity of the extraterritorial non-return extension.').

omega_variable(
    doctrine_practice_gap_direction,
    'Formal doctrine keeps expanding while pushbacks, interdiction, and externalization persist at scale: is the arrangement''s real force growing (entrenchment outrunning evasion) or decaying (practice drifting toward the restrictive sibling faster than doctrine closes the gap)?',
    'Longitudinal pairing of border-monitoring and pushback documentation with the doctrinal-expansion rate; classify intervals by which curve dominates.',
    'Determines lifecycle drift direction: continued entrenchment supports the current hybrid trajectory; sustained practice decay predicts inertial hollowing (doctrine intact, enforcement empty) or outright migration toward the restrictive sibling''s constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_practice_gap_direction, empirical, 'Net direction of the compliance gap over the interval.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(refugee_convention_text__expansive_humanitarian_reading, 0, 48).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(refugee_expansive_tr_t0, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(refugee_expansive_tr_t0, observed).
narrative_ontology:measurement(refugee_expansive_tr_t6, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 6, 0.2).
narrative_ontology:measurement_basis(refugee_expansive_tr_t6, observed).
narrative_ontology:measurement(refugee_expansive_tr_t12, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 12, 0.23).
narrative_ontology:measurement_basis(refugee_expansive_tr_t12, observed).
narrative_ontology:measurement(refugee_expansive_tr_t18, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 18, 0.24).
narrative_ontology:measurement_basis(refugee_expansive_tr_t18, observed).
narrative_ontology:measurement(refugee_expansive_tr_t24, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 24, 0.26).
narrative_ontology:measurement_basis(refugee_expansive_tr_t24, observed).
narrative_ontology:measurement(refugee_expansive_tr_t30, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement_basis(refugee_expansive_tr_t30, observed).
narrative_ontology:measurement(refugee_expansive_tr_t36, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 36, 0.31).
narrative_ontology:measurement_basis(refugee_expansive_tr_t36, observed).
narrative_ontology:measurement(refugee_expansive_tr_t42, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 42, 0.31).
narrative_ontology:measurement_basis(refugee_expansive_tr_t42, observed).
narrative_ontology:measurement(refugee_expansive_tr_t48, refugee_convention_text__expansive_humanitarian_reading, theater_ratio, 48, 0.32).
narrative_ontology:measurement_basis(refugee_expansive_tr_t48, projected).

% Extraction over time
narrative_ontology:measurement(refugee_expansive_be_t0, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement_basis(refugee_expansive_be_t0, observed).
narrative_ontology:measurement(refugee_expansive_be_t6, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 6, 0.31).
narrative_ontology:measurement_basis(refugee_expansive_be_t6, observed).
narrative_ontology:measurement(refugee_expansive_be_t12, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 12, 0.38).
narrative_ontology:measurement_basis(refugee_expansive_be_t12, observed).
narrative_ontology:measurement(refugee_expansive_be_t18, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 18, 0.36).
narrative_ontology:measurement_basis(refugee_expansive_be_t18, observed).
narrative_ontology:measurement(refugee_expansive_be_t24, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 24, 0.38).
narrative_ontology:measurement_basis(refugee_expansive_be_t24, observed).
narrative_ontology:measurement(refugee_expansive_be_t30, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 30, 0.39).
narrative_ontology:measurement_basis(refugee_expansive_be_t30, observed).
narrative_ontology:measurement(refugee_expansive_be_t36, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 36, 0.46).
narrative_ontology:measurement_basis(refugee_expansive_be_t36, observed).
narrative_ontology:measurement(refugee_expansive_be_t42, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 42, 0.43).
narrative_ontology:measurement_basis(refugee_expansive_be_t42, observed).
narrative_ontology:measurement(refugee_expansive_be_t48, refugee_convention_text__expansive_humanitarian_reading, base_extractiveness, 48, 0.44).
narrative_ontology:measurement_basis(refugee_expansive_be_t48, projected).

% Suppression requirement over time
narrative_ontology:measurement(refugee_expansive_su_t0, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 0, 0.34).
narrative_ontology:measurement_basis(refugee_expansive_su_t0, observed).
narrative_ontology:measurement(refugee_expansive_su_t6, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 6, 0.38).
narrative_ontology:measurement_basis(refugee_expansive_su_t6, observed).
narrative_ontology:measurement(refugee_expansive_su_t12, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 12, 0.44).
narrative_ontology:measurement_basis(refugee_expansive_su_t12, observed).
narrative_ontology:measurement(refugee_expansive_su_t18, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 18, 0.47).
narrative_ontology:measurement_basis(refugee_expansive_su_t18, observed).
narrative_ontology:measurement(refugee_expansive_su_t24, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement_basis(refugee_expansive_su_t24, observed).
narrative_ontology:measurement(refugee_expansive_su_t30, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 30, 0.54).
narrative_ontology:measurement_basis(refugee_expansive_su_t30, observed).
narrative_ontology:measurement(refugee_expansive_su_t36, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 36, 0.58).
narrative_ontology:measurement_basis(refugee_expansive_su_t36, observed).
narrative_ontology:measurement(refugee_expansive_su_t42, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 42, 0.59).
narrative_ontology:measurement_basis(refugee_expansive_su_t42, observed).
narrative_ontology:measurement(refugee_expansive_su_t48, refugee_convention_text__expansive_humanitarian_reading, suppression_requirement, 48, 0.6).
narrative_ontology:measurement_basis(refugee_expansive_su_t48, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(refugee_convention_text__expansive_humanitarian_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__restrictive_sovereignty_reading).
narrative_ontology:affects_constraint(refugee_convention_text__expansive_humanitarian_reading, refugee_convention_text__procedural_integrity_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'the Refugee Convention' decomposes, per the epsilon-invariance principle, into three structurally distinct constraints sharing one fixed text: this expansive-humanitarian instantiation (broad victim set reaching generalized violence, non-state actors, and gender/LGBTQ+/clan social groups; extraterritorial non-return covering interdiction and offshore processing; substantive-assessment duty), a restrictive-sovereignty instantiation (narrow individualized threshold, maximal sovereign discretion), and a procedural-integrity instantiation (process primacy, outcome secondary). Each carries its own epsilon, victim set, and enforcement object; pressure between them runs through litigation and treaty-body practice. This file links both siblings; the family is closed by reciprocal edges in the sibling files.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
