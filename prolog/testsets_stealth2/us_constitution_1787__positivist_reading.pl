% ============================================================================
% CONSTRAINT STORY: us_constitution_1787__positivist_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_us_constitution_1787__positivist_reading, []).

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
 *   constraint_id: us_constitution_1787__positivist_reading
 *   human_readable: Positivist Reading of the U.S. Constitution: Meaning Is Text Plus Amendments, Judiciary Confined to Text
 *   domain: legal/political
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the contested kernel
 *   us_constitution_1787: the positivist reading, on which constitutional
 *   meaning consists of the enacted text plus duly ratified amendments and
 *   nothing else, with judicial interpretation confined to that text. The
 *   standing arrangement under contest is the text-plus-amendments regime
 *   itself, and epsilon is authored for that regime as the positivist reading
 *   sees it — not for the rival regimes the sibling readings would install.
 *   The arrangement carries a genuine coordination function (one publicly
 *   knowable supreme law, change funneled through a codified supermajority
 *   procedure) and a real asymmetric transfer (constitutional-change monopoly
 *   conferred on coalitions that can clear Article V; the cost of textual
 *   silence borne by those who cannot amend). Per the epsilon-invariance
 *   principle, the kernel decomposes into three structurally distinct
 *   constraints — this file, the originalist reading, and the living reading
 *   — linked through network.affects_constraints rather than averaged into
 *   one story. KEY AGENTS (by structural relationship): - federal_judiciary:
 *   Administering seat (institutional/identity_locked) — applies and polices
 *   the text-bound rule; surrenders the discretion an unconstrained bench
 *   would hold - article_v_supermajority_coalitions: Primary beneficiary
 *   (powerful/mobile) — hold exclusive control of constitutional revision -
 *   congressional_legislative_majors: Beneficiary (powerful/constrained) —
 *   statutes shielded from principle-based judicial revision -
 *   state_legislatures: Beneficiary (institutional/constrained) — co-authors
 *   of meaning via the ratification gate - ordinary_citizens: Beneficiary
 *   with payer underside (moderate/constrained) — readable supreme law;
 *   supermajority-gated access to change - unenumerated_rights_claimants:
 *   Primary target (powerless/trapped) — bear the cost of textual silence
 *   with no amendable path - numerical_minority_groups: Target
 *   (powerless/trapped) — accumulate the cost of every protection the text
 *   failed to anticipate - living_constitution_advocates: Excluded voice
 *   (organized/arbitrage) — defined out of the framework's conversation,
 *   active in adjacent forums - constitutional_theory_community: Analytical
 *   observer (analytical/analytical) — maps the divergence between professed
 *   textual fidelity and practiced interpretation
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_constitution_1787__positivist_reading, 0.32).
domain_priors:suppression_score(us_constitution_1787__positivist_reading, 0.58).
domain_priors:theater_ratio(us_constitution_1787__positivist_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, extractiveness, 0.32).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(us_constitution_1787__positivist_reading, resistance, 0.62).

% --- Constraint claim ---
narrative_ontology:constraint_claim(us_constitution_1787__positivist_reading, tangled_rope).
narrative_ontology:human_readable(us_constitution_1787__positivist_reading, "Positivist Reading of the U.S. Constitution: Meaning Is Text Plus Amendments, Judiciary Confined to Text").
narrative_ontology:topic_domain(us_constitution_1787__positivist_reading, "legal/political").

domain_priors:requires_active_enforcement(us_constitution_1787__positivist_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(us_constitution_1787__positivist_reading, '1d63d66b-5c36-4898-ab14-449a6af38966').
narrative_ontology:cs_kernel_codification('1d63d66b-5c36-4898-ab14-449a6af38966', fixed_text).
narrative_ontology:cs_authority_grounding('1d63d66b-5c36-4898-ab14-449a6af38966', lineage).
narrative_ontology:cs_interpretation_layer_present('1d63d66b-5c36-4898-ab14-449a6af38966').
narrative_ontology:cs_reading_relation('1d63d66b-5c36-4898-ab14-449a6af38966', us_constitution_1787__originalist_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d63d66b-5c36-4898-ab14-449a6af38966', us_constitution_1787__living_reading, forecloses).
narrative_ontology:cs_axiom('1d63d66b-5c36-4898-ab14-449a6af38966', foundational, text_plus_amendments_exclusive_meaning_sources).
narrative_ontology:cs_axiom_status(text_plus_amendments_exclusive_meaning_sources, holdable).
narrative_ontology:cs_axiom_grounding('1d63d66b-5c36-4898-ab14-449a6af38966', text_plus_amendments_exclusive_meaning_sources, conventional).
narrative_ontology:cs_axiom('1d63d66b-5c36-4898-ab14-449a6af38966', foundational, judicial_discretion_subordinate_to_enacted_text).
narrative_ontology:cs_axiom_status(judicial_discretion_subordinate_to_enacted_text, holdable).
narrative_ontology:cs_axiom_grounding('1d63d66b-5c36-4898-ab14-449a6af38966', judicial_discretion_subordinate_to_enacted_text, conventional).
narrative_ontology:cs_axiom('1d63d66b-5c36-4898-ab14-449a6af38966', secondary, amendment_process_primary_change_channel).
narrative_ontology:cs_axiom_status(amendment_process_primary_change_channel, holdable).
narrative_ontology:cs_axiom_grounding('1d63d66b-5c36-4898-ab14-449a6af38966', amendment_process_primary_change_channel, conventional).
narrative_ontology:cs_reference_frame('1d63d66b-5c36-4898-ab14-449a6af38966', enacted_text_plus_amendments_canon).
narrative_ontology:cs_drift_state('1d63d66b-5c36-4898-ab14-449a6af38966', contemporary_doctrinal_practice, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1d63d66b-5c36-4898-ab14-449a6af38966', '').
narrative_ontology:cs_kernel_id(us_constitution_1787__positivist_reading, us_constitution_1787).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, article_v_supermajority_coalitions).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, congressional_legislative_majors).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, state_legislatures).
narrative_ontology:constraint_beneficiary(us_constitution_1787__positivist_reading, ordinary_citizens).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, unenumerated_rights_claimants).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, numerical_minority_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, federal_judiciary).
narrative_ontology:constraint_victim(us_constitution_1787__positivist_reading, ordinary_citizens).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Applies the enacted text to cases and polices the boundary of its own role: under this reading a judge may enforce what the words and duly ratified amendments say and nothing more. Day to day the bench administers the rule, deciding which arguments count as textual and which are ruled out as policy or aspiration. The price of administering it is the discretionary power an unconstrained bench would hold; leaving that role is not a career move available to a sitting judge, whose professional self-conception is built around fidelity to enacted law.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, federal_judiciary, agenda_setter,
    institutional, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, federal_judiciary, payer).

% Organized coalitions able to assemble two-thirds of both houses of Congress and three-fourths of the states. Because this reading makes the amendment process the only way constitutional meaning changes, whoever can clear that threshold holds exclusive control of constitutional revision. Such coalitions can also pursue ordinary legislation or litigation elsewhere, so the arrangement costs them little and hands them a monopoly they would not otherwise possess.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, article_v_supermajority_coalitions, beneficiary,
    powerful, generational, mobile, national).

% Enact statutes that a text-confined judiciary reviews only against the words Congress and the ratifiers actually adopted. Their products are insulated from judicial revision in the name of unenumerated principle, and their own institution's prerogatives are protected against judicial expansion. Their horizon is electoral, and their main lever on the arrangement is the confirmation power over the bench.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, congressional_legislative_majors, beneficiary,
    powerful, biographical, constrained, national).

% Hold the ratification gate on every constitutional amendment, making them co-authors of constitutional meaning under this reading. They gain standing relative to courts and Congress that an interpretation-free-for-all would erode. Their participation is episodic and their leverage concentrated in discrete ratification votes rather than continuous administration.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, state_legislatures, beneficiary,
    institutional, generational, constrained, national).

% Live under a supreme law they can read for themselves in a printed text, with change arriving only through a public, codified procedure rather than shifting judicial doctrine. When they find themselves in the minority on a contested question, however, their access to constitutional change runs exclusively through supermajority mobilization, which most citizen minorities cannot mount; their recourse is voting within a system whose fundamental terms they cannot individually move.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, ordinary_citizens, beneficiary,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(us_constitution_1787__positivist_reading, ordinary_citizens, payer).

% Seek protection for interests the text does not name in so many words — bodily autonomy, associational privacy, dignity-based equality. Under this reading no court may supply what the words omit, so their only lawful route is an amendment campaign requiring supermajorities they have no realistic path to assemble. They cannot exit the jurisdiction's constitutional order, and the arrangement leaves them bearing the full cost of textual silence.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, unenumerated_rights_claimants, payer,
    powerless, biographical, trapped, national).

% Small or dispersed populations whose fortunes depend on constitutional guarantees holding firm against local or temporary majorities. Textual guarantees protect them where the words are explicit; everywhere else, the reading routes their claims to an amendment threshold calibrated to exclude exactly their numbers. Across generations they accumulate the costs of every protection the text failed to anticipate.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, numerical_minority_groups, payer,
    powerless, generational, trapped, national).

% Scholars, practitioners, and movements who hold that constitutional meaning develops with society and that courts may articulate principles the text only gestures toward. Inside this reading's framework their position is not answered but defined out of lawful adjudication. They continue operating in adjacent forums — academy, bar associations, political movements, and the rival readings of the same text — which is where their influence goes instead.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, living_constitution_advocates, excluded,
    organized, biographical, arbitrage, national).

% Legal theorists, historians, and comparative scholars who map how the competing readings allocate authority and who document where professed textual fidelity and actual judicial practice diverge. They collect no rents and bear no burdens under the arrangement; their output is the record against which the reading's self-description can be checked.
narrative_ontology:constraint_stakeholder(us_constitution_1787__positivist_reading, constitutional_theory_community, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(us_constitution_1787__positivist_reading, article_v_supermajority_coalitions).
narrative_ontology:fixing_cost_class(us_constitution_1787__positivist_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a single publicly knowable body of supreme law: every official and citizen consults the same enacted text; the branches and the states coordinate on one fixed reference point; and constitutional change passes through one codified supermajority procedure, so fundamental law moves only when a broad cross-section of the polity consents.
% TRANSFER_FUNCTION: Moves interpretive authority from sitting judges to the enacted text and the Article V amending bodies; moves the practical cost of constitutional change onto groups unable to assemble supermajorities; and confers effective monopoly over constitutional revision on coalitions that can clear two-thirds of both houses and three-fourths of the states.
% ABSENT_VOICES: Living-constitution proponents are excluded by construction — the reading defines evolutionary interpretation out of lawful adjudication rather than engaging it on the merits. Unenumerated-rights claimants appear inside the framework only as amendment-seekers whose objections are met with threshold arithmetic. Neither voice participates in setting the interpretive agenda; both speak from forums this reading does not recognize as authoritative.
% DISAPPEARANCE_RATIONALE: If the text-bound norm vanished overnight, courts would resume gap-filling and principle-based adaptation immediately; the supermajority monopoly over constitutional change would dissolve; doctrinal authority would migrate from the enacted text to whatever the bench currently prizes; and the coalition that today controls revision would lose its exclusive channel. The allocation of constitutional-change power would rearrange around judicial practice within a generation.
% FOUNDING_PROBLEM: Design a supreme law strong enough to bind transient majorities and factions, yet remaining the people's own act: a written charter superior to ordinary statute, alterable only by a supermajority spanning the states — the 1787 problem of reconciling energetic government, separated powers, and popular sovereignty under fixed fundamental law.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the benefiting parties: Anti-Federalist ratification debates demanded written limits and a bill of rights as the price of assent; records of the state ratifying conventions attest the entrenchment-versus-revisability dilemma independently of any modern beneficiary; and comparative constitutional-design scholarship treats the same trade-off as a general problem every polity drafting a supreme law confronts.
narrative_ontology:disappearance_verdict(us_constitution_1787__positivist_reading, world_rearranges).
narrative_ontology:founding_problem_status(us_constitution_1787__positivist_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(us_constitution_1787__positivist_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(us_constitution_1787__positivist_reading, 'none', 1).
narrative_ontology:epsilon_provenance(us_constitution_1787__positivist_reading, 0.32, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_constitution_1787__positivist_reading_tests).
:- end_tests(us_constitution_1787__positivist_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is authored at 0.32: from the positivist seat the regime's authority flows from democratically enacted sources, so most of its operation is coordination cost rather than extraction; the residual is the amendment-threshold asymmetry and the irreducible judgment that survives inside 'what the text says.' Suppression is 0.58 and is authored as a raw structural property, unscaled by power or scope: the reading constitutively excludes evolutionary interpretation as a change channel and holds that exclusion in place through selection politics, doctrinal policing, and professional ethos. Theater is 0.22 — the rhetoric of mechanical application outruns practice, since hard cases force interpretive judgment the pure label disclaims, but the discipline is substantively operative, not vestigial. Accessibility_collapse is 0.60: within the frame, alternatives collapse considerably, but the frame itself remains a contestable choice among readings, so the figure sits well below natural-law levels. Resistance is 0.62: rival readings stay institutionally alive, and bench composition shifts repeatedly reopen the question. The claimed type (tangled_rope) is stated from structure — genuine coordination plus asymmetric transfer plus active enforcement — independently of these metric values; the engine computes per-seat classifications from the structural data, and any divergence between claim and computed type is the datum, not an error to reconcile. The temporal series run on one shared grid (six points, all three metrics at every point) showing monotone consolidation: enforcement capacity built steadily from the late-1950s reaction to adaptive judging, then plateaued as the textualist apparatus institutionalized; extraction and theater drifted up gently alongside it. No cyclical dynamics are asserted — the trajectory is consolidation, not oscillation.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the supermajority-coalition seat the arrangement is a subsidy: exclusive control of revision at trivial cost. From the trapped payer seats the identical structure operates as a closed door: every grievance the text does not name is answered with a threshold they cannot meet. The federal judiciary occupies a third position — it administers the rule and draws its professional identity from doing so, while surrendering the discretion the rival readings would restore, placing it near-symmetric rather than at either pole. Two actors at the same nominal power tier diverge sharply: state_legislatures (institutional, constrained) sit on the beneficiary side through the ratification gate, while the federal judiciary (institutional, identity_locked) pays in surrendered discretion — same power atom, opposite structural relationships, differentiated entirely by position relative to this specific constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: supermajority coalitions (mobile exit, generational horizon) sit nearest the beneficiary end; legislative majors and state legislatures follow; ordinary citizens sit moderately low, their secondary payer role pulling them toward symmetry. Declared victims derive high directionality: unenumerated-rights claimants and numerical minorities are powerless and trapped, placing them at the full-target end — trapped, identity-poor targets register nearer full target than mobile ones would. The federal judiciary declares no beneficiary/victim position, so its directionality takes the power-atom fallback near symmetry, which matches its dual situation as administrator and forfeitor of discretion; no override is authored because no structural declaration exists to correct. Living-constitution advocates are excluded rather than coordinated — their exclusion is part of what the enforcement machinery maintains, and they are routed to arbitrage in adjacent forums rather than into the derivation.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — binding fundamental law that remains the people's own revisable act — is live, so this is not a mandatrophy case: no sunset clause applies, the mandate has not outlived its function, and the arrangement is not maintained theatrically over a dead purpose. The tangled_rope classification guards against mislabeling in both directions. Reading the arrangement as a snare would erase the genuine coordination it delivers (a single knowable supreme law that all seats can consult) and would mistake a declared, openly contested design for concealed predation — the transfer to supermajority coalitions is the reading's advertised feature, argued over in public, not hidden behind a cover story. Reading it as a pure rope would erase the extraction: the amendment threshold systematically prices diffuse minorities out of constitutional change while remaining passable for organized coalitions, and that asymmetry rides through the very same structure that performs the coordination. Holding both facts in one classification is what the category is for.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is one reading of kernel us_constitution_1787 (positivist_reading). Which structural element fixes constitutional meaning — the enacted text plus amendments (this reading), the historical act of ratification (originalist_reading), or evolving social practice (living_reading) — and how does the answer redistribute the victim set?',
    'Not resolvable by data alone: the contest turns on which locus of meaning-determination a framework adopts. Resolution arrives through doctrinal and scholarly settlement, or through the engine''s cross-reading comparison of the three linked stories'' computed classifications.',
    'Adopting the living sibling removes the amendment-exclusive burden from unenumerated-rights claimants and lowers measured extraction on the trapped seats; adopting the originalist sibling adds historically displaced claimants and raises suppression of post-ratification meaning. This story''s classification holds only for the positivist instantiation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer-frame omega: one reading of a three-reading kernel; disagreement located at the locus of meaning-determination.').

omega_variable(
    plain_text_determinacy,
    'Does ''what the text says'' yield determinate content in hard cases without importing an interpretive theory of ambiguity, vagueness, and silence?',
    'Corpus study of canonically hard cases resolved avowedly ''by the text alone'': if convergent outcomes require background theory, the determinacy claim fails in exactly the cases that matter.',
    'If text underdetermines outcomes, part of the measured theater_ratio is irreducible rather than sloppiness, the text-bound discipline operates with a covert theoretical layer, and effective extraction on trapped payers rises because outcomes depend on unstated judgment while accountability runs to the text.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(plain_text_determinacy, conceptual, 'Whether the reading''s core premise survives contact with textual indeterminacy.').

omega_variable(
    amendment_threshold_incidence,
    'Does the Article V supermajority requirement in fact systematically exclude diffuse minorities while remaining passable for organized coalitions?',
    'Amendment-attempt success rates disaggregated by sponsor type; comparative entrenchment data across constitutions with varying amendment thresholds; historical incidence of rights secured by amendment versus adjudication.',
    'Confirms or refutes the extraction component of the tangled_rope classification: systematic exclusion establishes the asymmetric transfer; failure to find it would push the arrangement toward pure coordination and support a rope reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(amendment_threshold_incidence, empirical, 'Empirical incidence of the amendment threshold across sponsor types.').

omega_variable(
    restraint_durability_structural_vs_internalized,
    'Will the text-bound norm hold across appointment-coalition turnover, and is its enforcement structural (selection politics, doctrinal machinery) or internalized (professional ethos of fidelity to enacted law)?',
    'Track adherence across compositional shifts of the bench: if text-bound behavior persists when the appointing coalition flips, enforcement is structural; if it tracks personnel, the suppression is carried by internalized professional identity.',
    'Structural enforcement implies the arrangement''s suppression is durable infrastructure and the current classification is stable; internalized enforcement implies the constraint could relax rapidly under identity-frame change in the judiciary, moving the computed type toward scaffold-like transience.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(restraint_durability_structural_vs_internalized, empirical, 'Durability and mechanism of the text-bound enforcement norm across personnel change.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(us_constitution_1787__positivist_reading, 1958, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(us_c_tr_t1958, us_constitution_1787__positivist_reading, theater_ratio, 1958, 0.12).
narrative_ontology:measurement(us_c_tr_t1971, us_constitution_1787__positivist_reading, theater_ratio, 1971, 0.14).
narrative_ontology:measurement(us_c_tr_t1984, us_constitution_1787__positivist_reading, theater_ratio, 1984, 0.17).
narrative_ontology:measurement(us_c_tr_t1997, us_constitution_1787__positivist_reading, theater_ratio, 1997, 0.19).
narrative_ontology:measurement(us_c_tr_t2010, us_constitution_1787__positivist_reading, theater_ratio, 2010, 0.21).
narrative_ontology:measurement(us_c_tr_t2024, us_constitution_1787__positivist_reading, theater_ratio, 2024, 0.22).

% Extraction over time
narrative_ontology:measurement(us_c_be_t1958, us_constitution_1787__positivist_reading, base_extractiveness, 1958, 0.24).
narrative_ontology:measurement(us_c_be_t1971, us_constitution_1787__positivist_reading, base_extractiveness, 1971, 0.26).
narrative_ontology:measurement(us_c_be_t1984, us_constitution_1787__positivist_reading, base_extractiveness, 1984, 0.28).
narrative_ontology:measurement(us_c_be_t1997, us_constitution_1787__positivist_reading, base_extractiveness, 1997, 0.29).
narrative_ontology:measurement(us_c_be_t2010, us_constitution_1787__positivist_reading, base_extractiveness, 2010, 0.31).
narrative_ontology:measurement(us_c_be_t2024, us_constitution_1787__positivist_reading, base_extractiveness, 2024, 0.32).

% Suppression requirement over time
narrative_ontology:measurement(us_c_su_t1958, us_constitution_1787__positivist_reading, suppression_requirement, 1958, 0.3).
narrative_ontology:measurement(us_c_su_t1971, us_constitution_1787__positivist_reading, suppression_requirement, 1971, 0.38).
narrative_ontology:measurement(us_c_su_t1984, us_constitution_1787__positivist_reading, suppression_requirement, 1984, 0.47).
narrative_ontology:measurement(us_c_su_t1997, us_constitution_1787__positivist_reading, suppression_requirement, 1997, 0.53).
narrative_ontology:measurement(us_c_su_t2010, us_constitution_1787__positivist_reading, suppression_requirement, 2010, 0.57).
narrative_ontology:measurement(us_c_su_t2024, us_constitution_1787__positivist_reading, suppression_requirement, 2024, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(us_constitution_1787__positivist_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__originalist_reading).
narrative_ontology:affects_constraint(us_constitution_1787__positivist_reading, us_constitution_1787__living_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition of kernel us_constitution_1787 per the epsilon-invariance principle. The colloquial label 'the Constitution's meaning' conflates three structurally distinct claims: meaning fixed at ratification (originalist_reading), meaning evolving with society (living_reading), and meaning constituted by enacted text plus democratic amendments with the judiciary confined thereto (this file). Each story carries its own epsilon over its own standing arrangement: this story authors epsilon for the text-plus-amendments regime as the positivist reading sees it (low-moderate, 0.32); the originalist sibling authors epsilon for the ratification-fixed regime; the living sibling authors epsilon for the evolving-meaning regime. Victim sets differ accordingly. The upstream/downstream pressure runs between siblings through shared doctrinal terrain — textualist method built for this reading supplies tooling the originalist sibling deploys, and living-reading precedents are the practice-drift this reading's enforcement machinery exists to suppress — hence the family links in affects_constraints rather than isolated stories.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
