% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_plural_marriage_mandate__institutional_pragmatism_reading, []).

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
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto as Strategic Institutional Adaptation (Institutional Pragmatism Reading)
 *   domain: religious institutional history / commitment systems / political theology
 *
 * SUMMARY:
 *   Between 1887 and 1890 the federal campaign against plural marriage
 *   reached its apex: the church was disincorporated, its property escheated,
 *   its leaders imprisoned or in hiding, its members disfranchised. In
 *   October 1890 Wilford Woodruff issued the Manifesto, publicly suspending
 *   new plural marriages and presenting the suspension as divine revelation.
 *   The institutional pragmatism reading instantiated here holds that the
 *   revelation narrative is the legitimation surface of a survival-driven
 *   capitulation: doctrine remained canonically unchanged, public practice
 *   stopped, and new plural marriages continued under private authorization
 *   until the Second Manifesto of 1904. The M-set gap - doctrine unchanged,
 *   practice suspended, secret continuations - is this reading's primary
 *   observable. The standing arrangement under contest, assessed by this
 *   reading's lights, coordinates genuine collective survival while
 *   extracting from plural families required to dissolve commanded bonds and
 *   from members whose trust financed the public denials. This file is ONE
 *   reading of a three-reading kernel; the endogenous and exogenous readings
 *   are separate constraints linked through the network block, never folded
 *   into this one.
 *
 * KEY AGENTS:
 *   - - lds_first_presidency_and_apostles: agenda-setter and principal beneficiary (institutional/identity_locked) - authors the public declaration, manages the doctrine-practice gap, collects institutional survival and restored political standing
 *   - - federal_government_prosecutors: coercive counterparty turned beneficiary (institutional/mobile) - supplies the superior force that makes capitulation rational, collects formal compliance
 *   - - pre_manifesto_plural_families: primary payer (moderate/trapped) - bears the cost of ceasing commanded practice and dividing households
 *   - - secret_post_manifesto_couples: primary payer (powerless/trapped) - bear the cost of the gap between public denial and private authorization
 *   - - rank_and_file_membership: dual-positioned beneficiary/payer (organized/constrained) - receives peace, amnesty, and statehood; pays in epistemic trust
 *   - - deceived_rank_and_file_believers: payer (powerless/identity_locked) - organize their lives around official denials the leadership knew to be false
 *   - - principled_objectors_within_church: excluded voice (moderate/constrained) - hold that a genuine command cannot be recalled by expediency; marginalized from the decision
 *   - - religious_historians: analytical observer (analytical/analytical) - reconstruct the episode from diaries, minutes, and court records across the whole period
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.74).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.72).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.74).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.55).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto as Strategic Institutional Adaptation (Institutional Pragmatism Reading)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious institutional history / commitment systems / political theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, 'a1da7547-3fb2-4e22-ba09-15fb485b9e98').
narrative_ontology:cs_kernel_codification('a1da7547-3fb2-4e22-ba09-15fb485b9e98', fixed_text).
narrative_ontology:cs_authority_grounding('a1da7547-3fb2-4e22-ba09-15fb485b9e98', lineage).
narrative_ontology:cs_interpretation_layer_present('a1da7547-3fb2-4e22-ba09-15fb485b9e98').
narrative_ontology:cs_reading_relation('a1da7547-3fb2-4e22-ba09-15fb485b9e98', plural_marriage_mandate__endogenous_reinterpretation_reading, forecloses).
narrative_ontology:cs_reading_relation('a1da7547-3fb2-4e22-ba09-15fb485b9e98', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('a1da7547-3fb2-4e22-ba09-15fb485b9e98', foundational, revelation_narrative_functions_as_legitimation_cover).
narrative_ontology:cs_axiom_status(revelation_narrative_functions_as_legitimation_cover, holdable).
narrative_ontology:cs_axiom_grounding('a1da7547-3fb2-4e22-ba09-15fb485b9e98', revelation_narrative_functions_as_legitimation_cover, empirically_contingent).
narrative_ontology:cs_axiom('a1da7547-3fb2-4e22-ba09-15fb485b9e98', secondary, institutional_survival_ordered_above_doctrinal_consistency).
narrative_ontology:cs_axiom_status(institutional_survival_ordered_above_doctrinal_consistency, holdable).
narrative_ontology:cs_axiom_grounding('a1da7547-3fb2-4e22-ba09-15fb485b9e98', institutional_survival_ordered_above_doctrinal_consistency, empirically_contingent).
narrative_ontology:cs_reference_frame('a1da7547-3fb2-4e22-ba09-15fb485b9e98', managed_doctrine_practice_alignment).
narrative_ontology:cs_drift_state('a1da7547-3fb2-4e22-ba09-15fb485b9e98', post_manifesto_gap_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('a1da7547-3fb2-4e22-ba09-15fb485b9e98', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency_and_apostles).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_membership).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, federal_government_prosecutors).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, pre_manifesto_plural_families).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, secret_post_manifesto_couples).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_rank_and_file_believers).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_membership).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, living_oracle_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival_imperative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Issue the public declaration ending new plural marriages, present it as divine revelation, manage the distance between the public teaching and continued private solemnizations, negotiate with federal authorities for amnesty and statehood, and discipline members whose questions threaten the public account. Their offices, standing, and life's work are bound up with the institution's continuity; stepping outside the arrangement would mean surrendering the authority structure they personally embody.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency_and_apostles, agenda_setter,
    institutional, generational, identity_locked, continental).

% Drove the anti-polygamy campaign through confiscation, imprisonment, and disfranchisement until the church's declaration delivered formal compliance. Afterwards they largely stood down, resuming pressure only when evidence of continued marriages surfaced in the Smoot hearing era. They collect the concession: the practice they were empowered to eradicate formally ceased, at low administrative cost.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government_prosecutors, beneficiary,
    institutional, generational, mobile, national).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, federal_government_prosecutors, agenda_setter).

% Men and women married under the pre-1890 practice who were required to cease cohabitation, divide households, and carry criminal liability for prior covenants. Some had been imprisoned or lived underground during the raids; the declaration asked them to treat sacred family bonds as suspended. Leaving the community meant losing everything; staying meant dissolving or hiding their families.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, pre_manifesto_plural_families, payer,
    moderate, biographical, trapped, regional).

% Entered new plural marriages between 1890 and 1904 on private assurances from church leaders that the practice continued by divine sanction despite the public declaration. When the gap surfaced they faced prosecution, public denial of their marriages' validity, and eventual severance from the community whose officers had authorized their weddings.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, secret_post_manifesto_couples, payer,
    powerless, biographical, trapped, regional).

% Received peace, amnesty, restored voting rights, and eventually statehood as the crisis closed. They sustained the declaration in conference and rebuilt civic standing. They also absorbed the doctrinal whiplash of a command reversed and, unknowingly, lent their testimony and labor to public accounts that concealed continued marriages.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_membership, beneficiary,
    organized, biographical, constrained, continental).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_membership, payer).

% Converts and lifelong members who organized their lives around the official teaching that plural marriage had ended - marrying monogamously, emigrating, testifying before tribunals - while church officers performed new plural marriages in secret. Their trust in official denial is the resource the public account spent; discovering the gap forces a reckoning with everything built upon it.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_rank_and_file_believers, payer,
    powerless, biographical, identity_locked, continental).

% Members and a few officeholders who held that a genuine command of God cannot be withdrawn by expediency, or that the public account could not be squared with known facts. They raised objections in councils and private correspondence, were passed over, released, or eased to the margins, and had no vote on the course taken.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, principled_objectors_within_church, excluded,
    moderate, biographical, constrained, regional).

% Reconstruct the episode from diaries, council minutes, sealing records, and court files, comparing the official record against the private one across the whole period. They sit outside the community's accountability structures and owe no allegiance to either the devotional or the prosecutorial account.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, religious_historians, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, lds_first_presidency_and_apostles).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solved a simultaneous-demobilization problem under duress: a dispersed covenant community in which no household could safely stop practicing alone needed a single authoritative signal that the practice had ended for everyone at once, preserving communal cohesion and the institution's legal existence through the transition.
% TRANSFER_FUNCTION: Moved the costs of capitulation downward and outward: plural households surrendered marriages and cohabitation; ordinary members surrendered epistemic trust through public denials concealing continued sealings; in exchange the institution recovered legal standing, amnestied leaders regained political rights, and the membership received peace and eventual statehood.
% ABSENT_VOICES: Plural wives had no seat where the decision was made - the women whose marriages were suspended or rendered clandestine were objects of the announcement, not participants in it. Dissenting insiders who held that a genuine command cannot be recalled were heard in councils but excluded from the outcome. Post-Manifesto plural wives, married on private assurances, had no forum at all once the public account hardened.
% DISAPPEARANCE_RATIONALE: Without the declaration and its legitimating frame, the institution faced disincorporation, escheated property, imprisoned leadership, and a choice between open defiance ending in organizational destruction or quiet dissolution of communal cohesion; American Mormonism reorganizes around whichever branch occurs. With it, the entire subsequent shape of the tradition - statehood, the Smoot settlement, the twentieth-century identity - follows from the arrangement's terms.
% FOUNDING_PROBLEM: An existential federal campaign - disincorporation, property confiscation, electoral disfranchisement, mass imprisonment, leaders in hiding - that threatened the church's legal existence and physical cohesion unless plural marriage ceased.
% FOUNDING_PROBLEM_CORROBORATION: Federal statutes and Supreme Court records (the Edmunds-Tucker Act of 1887; Late Corporation of the Church of Jesus Christ of Latter-day Saints v. United States, 1890) attest the crisis and its severity independently of any church claim; the crisis's resolution is attested by the 1896 statehood act and the 1907 Senate seating of Reed Smoot. Historians outside the beneficiary set (B. Carmon Hardy, D. Michael Quinn, Kathleen Flake) corroborate both the founding problem and its death, while the church's own devotional literature keeps the problem rhetorically alive - the corroboration for 'dead' comes entirely from outside the benefiting parties.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.74, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(plural_marriage_mandate__institutional_pragmatism_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Epsilon is 0.74 with the referent fixed to the standing Manifesto arrangement (public suspension plus managed gap), assessed by this reading's own lights: two classes of concrete harm (commanded families dissolved under duress; couples married into a practice the leadership publicly denied performing) plus large-scale epistemic expenditure, offset partially by the real survival goods the arrangement purchased. Suppression is 0.72 as a RAW structural property - it is not scaled by power or scope; only extractiveness is scaled downstream by directionality and scope. Theater is 0.55: the revelation framing and the denial-management apparatus are a large and growing share of the arrangement's activity, though the underlying survival coordination is real. Accessibility_collapse is 0.60: open continuation was blocked by federal power, full doctrinal repudiation was unavailable without shattering the authority claims the institution rested on, and minority exit via schism remained possible (and was later taken). Resistance is 0.45: a community exhausted by the raids complied quietly; dissent was muted, private, and later channeled into the fundamentalist breakaway. The measurement series run on ONE shared eight-point grid (1890-1904, biennial) so every metric is authored at every examined time point. The suppression_requirement series is deliberately U-shaped because this story specifically tracks enforcement-capacity migration: federal enforcement winds down (amnesty, 1896 statehood) while internal enforcement of the public account builds (discipline of objectors, hardened denials under Smoot-hearing scrutiny, the 1904 Second Manifesto and subsequent prosecutions). Extractiveness and theater rise monotonically because deception ACCUMULATES - each year of secret solemnization adds couples whose marriages the public account denies. Metrics are measured at the accumulation-phase endpoint, not the crisis phase.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute divergent types from identical structural data. From the leadership seat the arrangement is necessary stewardship - the alternative was organizational annihilation - and with d near the beneficiary end its effective extraction is damped toward subsidy. From the trapped payer seats (plural families, secret couples) the same structure operates as enforced abandonment and betrayal, amplified by trapped exit. The federal seat experiences mission accomplished at minimal cost. The deceived-believer seat experiences the arrangement only retrospectively, when the gap surfaces. The engine computes this per-seat divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries: the leadership collects survival, preserved offices, and restored political rights (d near the beneficiary end, pushed further by identity-lock binding them to their own benefit stream); the membership collects peace and statehood (low-to-moderate d, dual-positioned as payer); the federal government collects formal compliance (low d, mobile exit letting it re-escalate at will, as the Smoot hearings showed). Victims: pre-Manifesto plural families and secret post-Manifesto couples bear the direct costs with trapped exit (d near the full-target end); deceived believers bear epistemic costs with identity-locked exit. Continental scope amplifies effective extraction on the target seats because verification was genuinely hard - private ceremonies, sealed records, and jurisdictional distance are what let the gap persist for fourteen years. Note the coalition-prevention effect: the gap design itself fragments the victim classes (plural families stigmatized, secret couples compromised by their own deniable marriages, deceived believers unaware), so no coalition seat ever forms.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem - existential federal assault on the institution's legal existence - was dead by roughly 1896-1907 (statehood, amnesty, the Smoot settlement), yet the arrangement persists as permanent doctrinal settlement. The dead-problem-plus-world_rearranges mismatch is exactly the zombie/capture signature the R5 consumer cross-checks against the theater path, and it fits this reading: a survival measure legitimated as revelation congealed into timeless doctrine. The tangled_rope classification prevents mislabeling in both directions: a pure-rope reading (necessary survival coordination, nothing more) misses the deception extraction and the concentrated receipt of gains at the leadership seat; a pure-snare reading (extraction with coordination as cover) misses the genuine collective-action achievement - a dispersed covenant community demobilizing a commanded practice simultaneously, without general schism, under existential duress. The M-set gap is the hinge: the same structure that saved the institution is the structure that spent its members' trust.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contestation,
    'This file instantiates the institutional_pragmatism_reading of the plural_marriage_mandate kernel; how would the endogenous_reinterpretation_reading or the exogenous_override_reading change the structural facts authored here?',
    'Comparative authoring of the sibling reading files against the same documentary base; the disagreement is located in the causal status of the revelation narrative (genuine revealed suspension vs. legitimation cover vs. forced abandonment of a still-binding command) and in which seat counts as beneficiary.',
    'Under the endogenous reading the beneficiary set expands to the covenant community and epsilon falls toward a coordination-cost floor; under the exogenous reading federal power becomes the operative agenda-setter and the victim set shifts to adherents of a suppressed divine requirement. The tangled_rope verdict here is reading-indexed, not kernel-indexed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contestation, conceptual, 'Which reading of the 1890 Manifesto governs classification; committer structure routed to omega per the kernel-reading rules.').

omega_variable(
    secret_continuation_extent,
    'How extensive were the post-Manifesto plural marriages actually performed under private authorization between 1890 and 1904, and how widely were they known inside the leadership?',
    'Archival reconstruction from council minutes, sealing records, and federal prosecution files (the Hardy and Quinn corpora); reconciliation of the official denial record against the private performance record.',
    'A larger gap raises effective extraction on the deceived seats and validates the rising theater_ratio series; a materially smaller gap narrows the deception claim and pulls the arrangement toward a cleaner survival-coordination reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secret_continuation_extent, empirical, 'Magnitude of the doctrine-practice gap that constitutes the primary observable of this reading.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is member acquiescence in the Manifesto account produced by structural coercion (the federal enforcement backdrop) or by internalized prophetic deference that would persist after coercion ends?',
    'Post-1904 trajectory analysis: if deference to the official account persists and hardens once federal pressure lapses (as the Smoot-era settlement and twentieth-century retellings suggest), the internalized component is substantial.',
    'If substantially internalized, effective suppression exceeds the structural measure and predicts durable persistence of the arrangement independent of external enforcement capacity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Structural vs. internalized suppression mechanism in member compliance.').

omega_variable(
    leadership_benefit_concentration,
    'Did the gains of the arrangement concentrate at the leadership seat (offices preserved, authority structure intact, political rights restored to the governing cadre) or diffuse across the whole membership?',
    'Comparative positional analysis of leadership versus membership before and after the settlement: office retention, property recovery, political rehabilitation, versus distributed gains of peace and statehood.',
    'Concentration at the leadership seat supports a capture-leaning tangled_rope verging toward snare; broad diffusion supports a coordination-dominant reading closer to rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(leadership_benefit_concentration, empirical, 'Whether receipt of the arrangement''s gains is concentrated or diffuse.').

omega_variable(
    woodruff_revelation_sincerity,
    'Did Woodruff experience something he took to be revelation alongside the strategic use of the revelation frame, or was the frame wholly instrumental?',
    'Close psychological and documentary reading of the diaries and contemporaneous accounts; likely irresolvable at the evidentiary limit.',
    'A sincere experiential component would soften the legitimation-cover axiom and produce a hybrid with the endogenous reading; a wholly instrumental frame strengthens the foundational axiom as authored.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(woodruff_revelation_sincerity, conceptual, 'Irreducible sincerity ambiguity inside the legitimation-cover claim.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1904).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.38).
narrative_ontology:measurement_basis(plur_tr_t1890, observed).
narrative_ontology:measurement(plur_tr_t1892, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1892, 0.41).
narrative_ontology:measurement_basis(plur_tr_t1892, observed).
narrative_ontology:measurement(plur_tr_t1894, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1894, 0.44).
narrative_ontology:measurement_basis(plur_tr_t1894, observed).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.47).
narrative_ontology:measurement_basis(plur_tr_t1896, observed).
narrative_ontology:measurement(plur_tr_t1898, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1898, 0.49).
narrative_ontology:measurement_basis(plur_tr_t1898, observed).
narrative_ontology:measurement(plur_tr_t1900, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1900, 0.51).
narrative_ontology:measurement_basis(plur_tr_t1900, observed).
narrative_ontology:measurement(plur_tr_t1902, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1902, 0.53).
narrative_ontology:measurement_basis(plur_tr_t1902, observed).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.55).
narrative_ontology:measurement_basis(plur_tr_t1904, observed).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.6).
narrative_ontology:measurement_basis(plur_be_t1890, observed).
narrative_ontology:measurement(plur_be_t1892, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1892, 0.63).
narrative_ontology:measurement_basis(plur_be_t1892, observed).
narrative_ontology:measurement(plur_be_t1894, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1894, 0.66).
narrative_ontology:measurement_basis(plur_be_t1894, observed).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.67).
narrative_ontology:measurement_basis(plur_be_t1896, observed).
narrative_ontology:measurement(plur_be_t1898, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1898, 0.69).
narrative_ontology:measurement_basis(plur_be_t1898, observed).
narrative_ontology:measurement(plur_be_t1900, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1900, 0.71).
narrative_ontology:measurement_basis(plur_be_t1900, observed).
narrative_ontology:measurement(plur_be_t1902, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1902, 0.73).
narrative_ontology:measurement_basis(plur_be_t1902, observed).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.74).
narrative_ontology:measurement_basis(plur_be_t1904, observed).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.82).
narrative_ontology:measurement_basis(plur_su_t1890, observed).
narrative_ontology:measurement(plur_su_t1892, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1892, 0.74).
narrative_ontology:measurement_basis(plur_su_t1892, observed).
narrative_ontology:measurement(plur_su_t1894, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1894, 0.65).
narrative_ontology:measurement_basis(plur_su_t1894, observed).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.58).
narrative_ontology:measurement_basis(plur_su_t1896, observed).
narrative_ontology:measurement(plur_su_t1898, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1898, 0.56).
narrative_ontology:measurement_basis(plur_su_t1898, observed).
narrative_ontology:measurement(plur_su_t1900, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1900, 0.61).
narrative_ontology:measurement_basis(plur_su_t1900, observed).
narrative_ontology:measurement(plur_su_t1902, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1902, 0.67).
narrative_ontology:measurement_basis(plur_su_t1902, observed).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.72).
narrative_ontology:measurement_basis(plur_su_t1904, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% Constraint family decomposition per the epsilon-invariance principle: the colloquial label 'the 1890 Manifesto' covers three structurally distinct claims with different epsilon values, beneficiary sets, and failure modes. The endogenous reading (low-to-moderate epsilon, coordination-cost-like, community-wide beneficiary set), the exogenous reading (high epsilon, coercion of adherents to a live divine command, federal agenda-setter), and this institutional pragmatism reading (substantive epsilon, deception-based extraction, leadership-concentrated receipt) are authored as separate files sharing the kernel plural_marriage_mandate. This reading is downstream of the secret-continuation documentary record: the M-set gap is its primary observable, so its epsilon stands or falls on evidence the sibling readings treat as noise (endogenous) or as irrelevant to the command's status (exogenous). All three files link one another through network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
