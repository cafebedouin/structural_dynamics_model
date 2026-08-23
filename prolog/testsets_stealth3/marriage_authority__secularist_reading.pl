% ============================================================================
% CONSTRAINT STORY: marriage_authority__secularist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-05
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority__secularist_reading, []).

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
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
 *   constraint_id: marriage_authority__secularist_reading
 *   human_readable: Secularist Reading: Legislative Monopoly over Marriage Authority and the UCC Elimination Mandate
 *   domain: legal_pluralism/constitutional_law/comparative_family_law
 *
 * SUMMARY:
 *   In a post-colonial constitutional democracy (paradigm case: India), the
 *   secularist reading of marriage authority holds that legitimate power to
 *   define marriage, divorce, and family succession belongs exclusively to
 *   the democratically elected legislature, and that the surviving system of
 *   community-specific personal laws is a transitional anomaly awaiting
 *   elimination through a Uniform Civil Code. This story instantiates THAT
 *   reading only, as a clean epsilon-invariant constraint: the arrangement
 *   modeled is the constitutionalized transitional regime itself — plural
 *   personal laws administered today under a standing directive that they
 *   await legislative abolition — with the secular-modernist program as its
 *   operating force. The reading endorses the destination (uniform law) while
 *   authoring the extraction of the transit: even on its own lights, the
 *   mandate operates coercively before it delivers anything, marking minority
 *   legal orders as provisional and subjecting their continuation to majority
 *   vote. The ε referent is this standing arrangement under contest, never
 *   the uniform arrangement the reading would install (which would drive ε
 *   toward zero for every advocacy reading and destroy the measurement).
 *   Sibling readings of the marriage_authority kernel are separate
 *   constraints in separate files; they are not described, hedged, or
 *   averaged here. KEY AGENTS (by structural relationship): -
 *   democratic_legislature: Agenda setter (institutional/arbitrage) — holds
 *   and exercises the claimed monopoly on family-law authorship -
 *   secular_modernist_coalition: Primary beneficiary (organized/mobile) —
 *   collects legitimacy and platform rents from the uniformity program -
 *   minority_religious_communities: Primary target (organized/constrained) —
 *   bears the elimination of self-governed family law -
 *   religious_judicial_leadership: Secondary target and incumbent
 *   administrator (organized/identity_locked) — loses adjudicative office
 *   under the program it currently staffs - minority_womens_organizations:
 *   Excluded voice (organized/constrained) — conditional constituency outside
 *   both platforms - constitutional_judiciary: Analytical observer
 *   (institutional/analytical) — shapes climate, controls neither content nor
 *   timetable
 *
 * KEY AGENTS:
 *   - democratic_legislature: agenda setter and structural beneficiary (institutional/arbitrage) — authors family-law statutes under a standing uniformity directive it alone can execute
 *   - secular_modernist_coalition: primary beneficiary (organized/mobile) — collects the nation-building narrative and electoral identity the uniformity platform generates
 *   - minority_religious_communities: primary payer (organized/constrained) — their codified family law stands publicly scheduled for replacement by a majority-weighted code
 *   - religious_judicial_leadership: payer with incumbent-administrator duality (organized/identity_locked) — clerical jurists whose office is constituted by the codes slated for abolition
 *   - minority_womens_organizations: excluded voice (organized/constrained) — conditional reform constituency consulted late and thinly
 *   - constitutional_judiciary: analytical observer (institutional/analytical) — adjudicates rights collisions and urges uniformity without legislating it
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__secularist_reading, 0.74).
domain_priors:suppression_score(marriage_authority__secularist_reading, 0.64).
domain_priors:theater_ratio(marriage_authority__secularist_reading, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, suppression_requirement, 0.64).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(marriage_authority__secularist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__secularist_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority__secularist_reading, "Secularist Reading: Legislative Monopoly over Marriage Authority and the UCC Elimination Mandate").
narrative_ontology:topic_domain(marriage_authority__secularist_reading, "legal_pluralism/constitutional_law/comparative_family_law").

domain_priors:requires_active_enforcement(marriage_authority__secularist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__secularist_reading, '99c54e1a-5d1c-4110-a003-3688df8b9860').
narrative_ontology:cs_kernel_codification('99c54e1a-5d1c-4110-a003-3688df8b9860', fixed_text).
narrative_ontology:cs_authority_grounding('99c54e1a-5d1c-4110-a003-3688df8b9860', lineage).
narrative_ontology:cs_interpretation_layer_present('99c54e1a-5d1c-4110-a003-3688df8b9860').
narrative_ontology:cs_reading_relation('99c54e1a-5d1c-4110-a003-3688df8b9860', marriage_authority__communal_autonomy_reading, forecloses).
narrative_ontology:cs_reading_relation('99c54e1a-5d1c-4110-a003-3688df8b9860', marriage_authority__federalist_millet_reading, forecloses).
narrative_ontology:cs_reading_relation('99c54e1a-5d1c-4110-a003-3688df8b9860', marriage_authority__gender_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('99c54e1a-5d1c-4110-a003-3688df8b9860', marriage_authority__judicial_harmonization_reading, influences).
narrative_ontology:cs_axiom('99c54e1a-5d1c-4110-a003-3688df8b9860', foundational, legislative_exclusivity_over_marriage_authority).
narrative_ontology:cs_axiom_status(legislative_exclusivity_over_marriage_authority, holdable).
narrative_ontology:cs_axiom_grounding('99c54e1a-5d1c-4110-a003-3688df8b9860', legislative_exclusivity_over_marriage_authority, conventional).
narrative_ontology:cs_axiom('99c54e1a-5d1c-4110-a003-3688df8b9860', foundational, personal_law_pluralism_transitional_anomaly).
narrative_ontology:cs_axiom_status(personal_law_pluralism_transitional_anomaly, holdable).
narrative_ontology:cs_axiom_grounding('99c54e1a-5d1c-4110-a003-3688df8b9860', personal_law_pluralism_transitional_anomaly, empirically_contingent).
narrative_ontology:cs_axiom('99c54e1a-5d1c-4110-a003-3688df8b9860', secondary, uniform_code_completes_citizenship_equality).
narrative_ontology:cs_axiom_status(uniform_code_completes_citizenship_equality, holdable).
narrative_ontology:cs_axiom_grounding('99c54e1a-5d1c-4110-a003-3688df8b9860', uniform_code_completes_citizenship_equality, instrumental).
narrative_ontology:cs_reference_frame('99c54e1a-5d1c-4110-a003-3688df8b9860', parliamentary_uniform_family_law).
narrative_ontology:cs_drift_state('99c54e1a-5d1c-4110-a003-3688df8b9860', contemporary_personal_law_regime, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('99c54e1a-5d1c-4110-a003-3688df8b9860', '2026-08-05T09:30:00Z').
narrative_ontology:cs_kernel_id(marriage_authority__secularist_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, democratic_legislature).
narrative_ontology:constraint_beneficiary(marriage_authority__secularist_reading, secular_modernist_coalition).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, minority_religious_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(marriage_authority__secularist_reading, religious_judicial_leadership).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the constitutional charge (a directive-principle article) to move the country toward a uniform civil code, and writes every family-law statute that passes. Seats are allocated by population, so the majority community's governing coalition ordinarily controls what any uniform code would contain. It can suspend, accelerate, or reshape the uniformity project at will; nothing binds it except electoral consequence, and it can redirect the project's costs onto constituencies that did not set its agenda.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, democratic_legislature, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, democratic_legislature, beneficiary).

% Modernizing intellectuals, reform movements, ruling-party strategists, and urban professional classes who campaign for a single civil code. They gain a nation-building narrative, administrative simplification, and a distinctive electoral identity from advancing uniformity. Few of their members' own marriages, inheritances, or worship practices would change materially under the code they propose; if the political costs mount, they can pivot to other agendas without personal exposure.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, secular_modernist_coalition, beneficiary,
    organized, generational, mobile, national).

% Religious minorities (most prominently Muslims; also Christians and Parsis) currently order marriage, divorce, and succession through their own codified or customary law, administered by community institutions with state recognition. A uniform code drafted by the majority-weighted legislature would replace these codes outright. Individual members can emigrate or formally leave the faith, but the community as a legal collective has nowhere else to take its family law; its representative bodies read the standing uniformity directive as a published promise that their legal order will be abolished by majority vote.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_religious_communities, payer,
    organized, generational, constrained, national).

% Ulama councils, personal-law-board elites, and clerical jurists currently interpret and administer the communal family codes; their social authority rests on being the recognized custodians of that law. A uniform civil statute would strip away the adjudicative function their offices are built around. Defending the communal codes is not one position among others available to them — abandoning it would dissolve their own standing — and several have staked careers and biographies on resisting codification.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, religious_judicial_leadership, payer,
    organized, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__secularist_reading, religious_judicial_leadership, agenda_setter).

% Organizations of women from within minority communities press simultaneously for reform of discriminatory provisions inside their own communal codes and for enforceable safeguards in any future uniform code. They are consulted late and thinly in official drafting exercises; their conditional stance (equality guarantees first, uniformity second) fits neither the modernist nor the communal platform cleanly, and neither side has an incentive to amplify it.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, minority_womens_organizations, excluded,
    organized, biographical, constrained, national).

% The constitutional courts adjudicate collisions between fundamental rights and personal-law provisions, and periodically urge the legislature toward uniformity while declining to legislate it themselves. Their pronouncements shape the climate in which the uniformity project advances, but they control neither its content nor its timetable.
narrative_ontology:constraint_stakeholder(marriage_authority__secularist_reading, constitutional_judiciary, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: A uniform civil code solves real coordination problems: marriages and divorces crossing community lines currently fall between conflicting codes; differential rules invite forum-shopping; family courts fragment procedurally along confessional seams; and civil-status documentation varies by religion. One civil law gives every citizen the same predictable marriage, divorce, and succession regime regardless of confession.
% TRANSFER_FUNCTION: Moves authorship of family law from community religious institutions to the central legislature; moves normative default-setting from minority traditions to majority-weighted democratic process; moves adjudicative authority from clerical and community councils to state courts; and transfers the option-value of communal legal autonomy to the state.
% ABSENT_VOICES: Minority women's organizations sit outside the drafting conversation despite holding the most granular knowledge of what each code does to its least protected members (paired with the excluded stakeholder seat). Tribal customary authorities are absent by design — their exemption removes them from the negotiation entirely. Smaller denominations (Parsi, Jewish) lack legislative weight to be heard. Internal community dissenters have no channel that their own leadership or the modernist coalition both find convenient.
% DISAPPEARANCE_RATIONALE: If the uniformity directive and the legislative-monopoly doctrine vanished overnight, the field would reorganize around negotiated pluralism: communities would codify or renegotiate their codes, the judicial-harmonization route would proceed case by case, inter-community dispute-coordination gaps would reopen and be patched ad hoc. Nothing collapses — but the entire architecture of family-law debate, and the modernist coalition's central political identity, are built on this arrangement's existence.
% FOUNDING_PROBLEM: The post-colonial state inherited a patchwork of confessional family codes from colonial indirect-rule administration, which the founders read as an obstacle to unified citizenship; the constitution therefore carried a directive principle instructing the state to endeavor toward a uniform civil code.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated as live by the modernist coalition and successive government consultation papers; corroborated as substantially transformed-or-never-urgent by sources outside the benefiting parties — most notably the 21st Law Commission's 2016 consultation paper, an official body independent of the uniformity coalition, which concluded a uniform code was 'neither necessary nor desirable at this stage,' alongside comparative-family-law scholarship and minority community testimony arguing that unity and uniformity are distinct goods. No party outside the coalition attests the problem as urgent in its original form.
narrative_ontology:disappearance_verdict(marriage_authority__secularist_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__secularist_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__secularist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__secularist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__secularist_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority__secularist_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority__secularist_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority__secularist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.74 at interval end) because the arrangement transfers family-law authorship from communities to a legislature structurally controlled by the majority coalition, and because the standing directive taxes minority communities continuously — every year of 'transition' is a year their legal order is officially provisional. Suppression is substantial but below extraction (0.64) and is STRUCTURAL, not scaled: it consists of the statutory override machinery, the directive's standing threat, and the demonstrated willingness to legislate against community protest (the post-Shah Bano counter-statute of 1986), not of any interpersonal mechanism. Theater follows a documented arc on the shared eight-point grid: low during the founding decade when uniformity was a genuine drafting program (0.12), peaking in the symbolic-mobilization era when the UCC functioned chiefly as an electoral plank without a bill (0.43 circa 2010), then easing (0.34) as actual enactment (state-level uniform code, 2024-25) restored functional content. Extractiveness and suppression rise monotonically across the interval — enforcement infrastructure matured from a dormant directive into enacted codes requiring compliance machinery — so no cyclical interpretation is offered; the 1985-86 flashpoint is a step-change in the trend, not an oscillation cycle. Claim/metric independence is maintained: claimed_type is tangled_rope because the structure genuinely possesses both legs (real coordination function in uniform civil order; asymmetric extraction through majoritarian default-setting), and the metrics are authored as descriptive truths independently — the engine computes per-seat classifications from the structural data, and divergence between the claim and any seat's computed type is signal, not error.
 *
 * PERSPECTIVAL GAP:
 *   Seats should compute very differently. From the legislature's and coalition's positions the arrangement is a nation-building coordination project they champion: the coalition seat experiences something close to rope. From the minority communities' position the same structure operates as scheduled dispossession with no collective exit — a heavily extractive seat pushed toward snare-flavored experience by their constrained exit options. The clergy seat is the sharpest divergence: identity_locked (their authority is constituted by the very codes under abolition), so the arrangement registers to them as existential, and effective extraction amplifies accordingly. The judiciary observes the whole without paying or collecting. The engine derives these per-seat classifications from power, exit, and directional data; this commentary predicts the shape of the divergence without adjudicating it.
 *
 * DIRECTIONALITY LOGIC:
 *   Declarations map to directionality as follows. democratic_legislature: agenda setter with listed beneficiary status and arbitrage-grade exit (it can always rewrite the rules) — sits near the beneficiary end of d. secular_modernist_coalition: pure beneficiary with mobile exit — nearest the subsidy end. minority_religious_communities: declared victims with constrained exit (individual apostasy or emigration exists; collective legal existence cannot relocate) — near the full-target end, with the constrained (rather than trapped) rating slightly damping effective extraction relative to a fully immobile population. religious_judicial_leadership: payers whose exit is identity_locked — the identity-fusion mechanism (institutional identity: the office IS the custodianship) places them at the extreme target end; if the identity frame broke (clergy reconstituting as private counselors rather than legal administrators), their d would drop toward that of ordinary community members. minority_womens_organizations are excluded rather than positioned: their absence informs the consensus-provenance check but contributes no directional data. No directionality overrides were needed — the derivation chain produces accurate d values from the declared beneficiary/victim structure and exit profiles.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (colonial-era confessional patchwork obstructing unified citizenship) is recorded as CONTESTED, not dead: the majority community's own code was reformed first (1955-56), a civil-marriage opt-out exists, and an official commission outside the beneficiary coalition has attested the remaining problem does not compel the prescribed solution. The mismatch consumer therefore finds status=contested x verdict=world_rearranges — no zombie flag — which is correct: the mandate has outlived its original urgency but retains live function (recent enactment proves the coordination leg is not theatrical residue), so this is not a piton candidate. The classification discipline matters in both directions here: the genuine coordination function (uniform legal order solving inter-confessional dispute coordination) blocks a pure-snare reading despite high extraction, and the asymmetric extraction (minority legal orders abolished by majority vote, coalition collecting the platform rents) blocks a pure-rope reading despite the modernists' sincerity. The theater trajectory documents how the symbol detached from the statute book for two decades — the classic Goodhart signature — while the recent return to enactment separates this constraint from merely performative maintenance. fixing_cost is prohibitive: the body that could abandon the mandate (the legislature) is the body whose coalition identity is built on it, and formally retracting a constitutional directive would cost more than dormancy does — hence the characteristic pattern of a constraint maintained by suspension rather than execution.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    sibling_reading_structural_delta,
    'This story instantiates exactly one reading (secularist_reading) of the marriage_authority kernel; how would each sibling reading change the seat structure, epsilon, and classification of the shared referent?',
    'Generate the four sibling stories (communal_autonomy_reading, federalist_millet_reading, gender_rights_reading, judicial_harmonization_reading) and compare per-seat classifications across the kernel; the zero-sum relation predicted here (foreclosure of the communal-autonomy and millet cores by legislative exclusivity) is confirmed or revised by cross-reading comparison.',
    'Under communal_autonomy_reading the beneficiary/victim structure inverts (communities coordinated, secular legislature as target); under gender_rights_reading the victim set shifts to women under ALL codes including reformed ones; epsilon diverges per reading over the fixed referent per the reading-indexed rule.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_structural_delta, conceptual, 'Committer-frame routing: this story''s position within the marriage_authority kernel and what siblings would change.').

omega_variable(
    modernization_convergence_premise,
    'Is the transitional-anomaly premise — that legal modernization converges on uniform state-authored family law — an empirical regularity of institutional development, or a constructed preference of the modernizing coalition presented as destiny?',
    'Comparative legal-historical analysis of durable legal pluralisms (consociational family-law regimes, surviving millet-style arrangements, indigenous-law federations) to establish whether convergence is general or coalition-relative.',
    'If convergence fails as a general pattern, the elimination mandate loses its telos justification and the constraint drifts toward pure extraction with a cover story; if convergence holds, part of the measured extraction is transition cost toward a stable coordination equilibrium.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(modernization_convergence_premise, empirical, 'Whether the anomaly-awaiting-elimination premise is empirically grounded or coalition-serving.').

omega_variable(
    intra_community_gender_allocation,
    'Does elimination of personal-law variation actually deliver the emancipation this reading claims for women under discriminatory communal provisions, or does majority-authored uniformity substitute a different patriarchy?',
    'Disaggregated outcome comparison for women under reformed uniform codes versus reformed communal codes (divorce access, maintenance enforcement, succession outcomes). This axis is the gender_rights sibling reading''s home terrain; this story records the boundary and routes the question rather than adjudicating it.',
    'Determines whether women-under-personal-laws are net beneficiaries (strengthening the coordination leg) or concealed payers (deepening the extraction leg); affects whether the declared beneficiary set extends beyond the modernist coalition.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intra_community_gender_allocation, empirical, 'Contested intra-community beneficiary claim at the boundary with the gender_rights sibling reading.').

omega_variable(
    majority_default_encoding,
    'Will the uniform code produced by a majority-weighted legislature encode Hindu-majority cultural defaults (as critics of the first enacted state-level code allege), converting legal uniformity into assimilation?',
    'Clause-level comparison of enacted uniform-code provisions against existing Hindu personal law; distributional analysis of carve-outs (tribal-area exemptions, ceremony requirements) and whom they protect.',
    'If default encoding is confirmed, effective extraction on minority seats amplifies sharply and the coordination function narrows to majoritarian standard-setting, pushing per-seat computation toward snare for the minority payer seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(majority_default_encoding, empirical, 'Whether uniformity is content-neutral coordination or majority-norm capture.').

omega_variable(
    transition_or_permanent_politics,
    'Is the awaiting-elimination framing a genuine sunset (achieve uniformity, then dissolve into ordinary law) or a renewable mobilization resource that persists regardless of progress made?',
    'Track whether the modernist coalition retires the uniformity plank after enactments or renews it indefinitely against residual exceptions (tribal exemptions, conversion disputes, ceremonial requirements).',
    'A genuine sunset would lend the arrangement scaffold character; renewable politics confirms persistent tangled_rope operation with rising theater as the symbol detaches from the statute book.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(transition_or_permanent_politics, conceptual, 'Whether the transitional framing carries real sunset logic or is a permanent political asset.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__secularist_reading, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t1950, marriage_authority__secularist_reading, theater_ratio, 1950, 0.12).
narrative_ontology:measurement(marr_tr_t1961, marriage_authority__secularist_reading, theater_ratio, 1961, 0.17).
narrative_ontology:measurement(marr_tr_t1972, marriage_authority__secularist_reading, theater_ratio, 1972, 0.21).
narrative_ontology:measurement(marr_tr_t1985, marriage_authority__secularist_reading, theater_ratio, 1985, 0.29).
narrative_ontology:measurement(marr_tr_t1996, marriage_authority__secularist_reading, theater_ratio, 1996, 0.37).
narrative_ontology:measurement(marr_tr_t2010, marriage_authority__secularist_reading, theater_ratio, 2010, 0.43).
narrative_ontology:measurement(marr_tr_t2018, marriage_authority__secularist_reading, theater_ratio, 2018, 0.41).
narrative_ontology:measurement(marr_tr_t2025, marriage_authority__secularist_reading, theater_ratio, 2025, 0.34).

% Extraction over time
narrative_ontology:measurement(marr_be_t1950, marriage_authority__secularist_reading, base_extractiveness, 1950, 0.42).
narrative_ontology:measurement(marr_be_t1961, marriage_authority__secularist_reading, base_extractiveness, 1961, 0.47).
narrative_ontology:measurement(marr_be_t1972, marriage_authority__secularist_reading, base_extractiveness, 1972, 0.53).
narrative_ontology:measurement(marr_be_t1985, marriage_authority__secularist_reading, base_extractiveness, 1985, 0.61).
narrative_ontology:measurement(marr_be_t1996, marriage_authority__secularist_reading, base_extractiveness, 1996, 0.66).
narrative_ontology:measurement(marr_be_t2010, marriage_authority__secularist_reading, base_extractiveness, 2010, 0.7).
narrative_ontology:measurement(marr_be_t2018, marriage_authority__secularist_reading, base_extractiveness, 2018, 0.72).
narrative_ontology:measurement(marr_be_t2025, marriage_authority__secularist_reading, base_extractiveness, 2025, 0.74).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t1950, marriage_authority__secularist_reading, suppression_requirement, 1950, 0.3).
narrative_ontology:measurement(marr_su_t1961, marriage_authority__secularist_reading, suppression_requirement, 1961, 0.34).
narrative_ontology:measurement(marr_su_t1972, marriage_authority__secularist_reading, suppression_requirement, 1972, 0.39).
narrative_ontology:measurement(marr_su_t1985, marriage_authority__secularist_reading, suppression_requirement, 1985, 0.51).
narrative_ontology:measurement(marr_su_t1996, marriage_authority__secularist_reading, suppression_requirement, 1996, 0.56).
narrative_ontology:measurement(marr_su_t2010, marriage_authority__secularist_reading, suppression_requirement, 2010, 0.6).
narrative_ontology:measurement(marr_su_t2018, marriage_authority__secularist_reading, suppression_requirement, 2018, 0.62).
narrative_ontology:measurement(marr_su_t2025, marriage_authority__secularist_reading, suppression_requirement, 2025, 0.64).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__secularist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__federalist_millet_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__secularist_reading, marriage_authority__judicial_harmonization_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the colloquial question 'who should govern marriage in a plural democracy' decomposes, per the epsilon-invariance principle, into five structurally distinct claims — one per reading of the marriage_authority kernel — each with its own epsilon, beneficiary/victim structure, and classification. This file is the secularist member. Upstream influence runs in both directions across the family: the secularist reading cites the modernization-convergence thesis as evidence for elimination, while the judicial_harmonization reading's accumulated case law supplies the constitutional-floor precedent the secularist program would codify. The communal_autonomy and federalist_millet readings are zero-sum counterparts whose cores this reading forecloses; the gender_rights reading shares the referent but relocates the axis of contestation inside communities, which is why the intra-community gender question is routed to that sibling (see omega intra_community_gender_allocation) rather than adjudicated here.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
