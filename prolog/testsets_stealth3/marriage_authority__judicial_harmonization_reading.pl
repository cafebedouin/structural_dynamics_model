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
 *   human_readable: Judicial Harmonization of Personal Law via Case-by-Case Constitutional Floor
 *   domain: legal/constitutional/family
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the marriage_authority kernel: the
 *   claim that marriage-norm authority legitimately evolves through
 *   case-by-case apex-court review, which imposes a constitutional floor
 *   across religious personal law codes without any comprehensive civil code
 *   ever being legislated. The mechanism is modeled on jurisdictions with
 *   constitutionally supreme fundamental rights operating over plural
 *   personal law systems. Per Rule 1, the contest between readings is NOT
 *   described inside the constraint: this file authors a single
 *   epsilon-invariant arrangement — the judicial floor mechanism itself —
 *   with its own beneficiaries, payers, metrics, and type. The expected
 *   structural delta from the manifest is honored: the reading describes an
 *   institutional mechanism rather than a freestanding normative creed, the
 *   arrangement is transitional (its justification is the convergence it
 *   produces, not a steady state), and the judiciary sits in the beneficiary
 *   column. Claim and metrics are independent authored facts: the type is
 *   claimed as scaffold because the mechanism's own logic terminates at
 *   convergence, while the metrics describe moderately extractive,
 *   increasingly enforced operation — the engine computes per-seat
 *   classifications from the structural data and measures any divergence.
 *
 * KEY AGENTS:
 *   - supreme_court_judiciary: agenda-setting beneficiary (institutional/constrained) — administers the floor, collects precedent authority with each ruling
 *   - vulnerable_women_under_personal_law: primary intended beneficiary (organized/trapped) — obtains enforceable remedies, cannot exit community membership
 *   - communal_religious_authorities: primary payer (powerful/identity_locked) — loses interpretive territory ruling by ruling
 *   - traditional_practice_adherents: payer (moderate/identity_locked) — individual practices reclassified as invalid
 *   - minority_personal_law_communities: dual-positioned payer-beneficiary (moderate/identity_locked) — protected and revised at once
 *   - national_legislature: excluded institutional actor (mobile) — retains plenary power, declines to exercise it
 *   - public_interest_litigators: beneficiary and agenda co-shaper (organized/mobile) — selects which inequities reach the court
 *   - comparative_family_law_scholars: analytical observer (analytical/analytical) — tracks convergence without stakes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority__judicial_harmonization_reading, 0.52).
domain_priors:suppression_score(marriage_authority__judicial_harmonization_reading, 0.45).
domain_priors:theater_ratio(marriage_authority__judicial_harmonization_reading, 0.22).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, extractiveness, 0.52).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, theater_ratio, 0.22).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(marriage_authority__judicial_harmonization_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority__judicial_harmonization_reading, scaffold).
narrative_ontology:human_readable(marriage_authority__judicial_harmonization_reading, "Judicial Harmonization of Personal Law via Case-by-Case Constitutional Floor").
narrative_ontology:topic_domain(marriage_authority__judicial_harmonization_reading, "legal/constitutional/family").

domain_priors:requires_active_enforcement(marriage_authority__judicial_harmonization_reading).
narrative_ontology:has_sunset_clause(marriage_authority__judicial_harmonization_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority__judicial_harmonization_reading, 'e2da0c32-a5fe-49f8-b26e-1b29b3de14ad').
narrative_ontology:cs_kernel_codification('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', fixed_text).
narrative_ontology:cs_authority_grounding('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', lineage).
narrative_ontology:cs_interpretation_layer_present('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad').
narrative_ontology:cs_reading_relation('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', marriage_authority__communal_autonomy_reading, influences).
narrative_ontology:cs_reading_relation('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', marriage_authority__secularist_reading, coexists_with).
narrative_ontology:cs_reading_relation('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', marriage_authority__gender_rights_reading, influences).
narrative_ontology:cs_reading_relation('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', marriage_authority__federalist_millet_reading, influences).
narrative_ontology:cs_axiom('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', foundational, constitutional_floor_binding_across_codes).
narrative_ontology:cs_axiom_status(constitutional_floor_binding_across_codes, holdable).
narrative_ontology:cs_axiom_grounding('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', constitutional_floor_binding_across_codes, conventional).
narrative_ontology:cs_axiom('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', foundational, incremental_convergence_preferred_to_legislation).
narrative_ontology:cs_axiom_status(incremental_convergence_preferred_to_legislation, holdable).
narrative_ontology:cs_axiom_grounding('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', incremental_convergence_preferred_to_legislation, instrumental).
narrative_ontology:cs_reference_frame('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', constitutional_floor_pluralism).
narrative_ontology:cs_drift_state('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', contemporary_post_landmark_rulings_era, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('e2da0c32-a5fe-49f8-b26e-1b29b3de14ad', '').
narrative_ontology:cs_kernel_id(marriage_authority__judicial_harmonization_reading, marriage_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, vulnerable_women_under_personal_law).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, public_interest_litigators).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, communal_religious_authorities).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, traditional_practice_adherents).
narrative_ontology:constraint_victim(marriage_authority__judicial_harmonization_reading, minority_personal_law_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority__judicial_harmonization_reading, minority_personal_law_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hears petitions challenging specific marriage, divorce, maintenance, and succession practices under religious personal law codes, rules on whether they satisfy constitutional guarantees, and issues binding precedents that lower courts apply across all communities. Each ruling enlarges the body of doctrine it administers and the class of disputes only it can resolve. It cannot decline the docket without abandoning its declared role as guarantor of fundamental rights, and its institutional standing compounds with each precedent it sets.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, agenda_setter,
    institutional, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary, beneficiary).

% Women whose marriages, maintenance, inheritance, and divorce are governed by religious personal law codes containing provisions that disadvantage them. They obtain enforceable remedies when a court strikes a discriminatory practice, but they remain members of the communities whose codes are revised — leaving would mean exiting family, faith, and inheritance networks together. They act collectively through rights organizations and public interest petitions rather than as isolated litigants.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, vulnerable_women_under_personal_law, beneficiary,
    organized, biographical, trapped, national).

% Personal law boards, clerical councils, and priestly hierarchies whose interpretive authority over marriage and family life is narrowed each time a ruling displaces a traditional practice with a constitutional requirement. Their standing rests on being custodians of an unbroken tradition; accepting that an external court may revise that tradition corrodes the source of their own position. They mobilize followers, campaign against rulings, and press the legislature for statutory reversal.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, communal_religious_authorities, payer,
    powerful, generational, identity_locked, national).

% Individuals whose marriages and family arrangements were lawful under their community's code as practiced, and who face invalidation or criminal exposure when a ruling reclassifies a practice — a unilateral divorce form, a plural marriage, an unequal succession share — as unconstitutional. Their attachment to these forms is bound up with religious identity and family standing, and adopting the reformed practice can carry community sanction of its own.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, traditional_practice_adherents, payer,
    moderate, biographical, identity_locked, national).

% Holds plenary power to codify a uniform civil code or to reform personal law statutes article by article, and has declined to exercise it for decades because any comprehensive bill would fracture the governing coalition. It occasionally reverses or blunts a judicial intervention with a targeted statute. Its voice in the ongoing revision of family law is largely reactive; the pace and content of change are set elsewhere.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, national_legislature, excluded,
    institutional, generational, mobile, national).

% Religious minorities whose personal law is the most frequent object of harmonization rulings. They receive the same constitutional protections as everyone else when a discriminatory practice falls, but they experience each intervention as external revision of communal life and read the cumulative pattern as preparation for absorption into a single code. Remaining under the community's legal order is not a choice they can resign from; it would mean leaving the community.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, minority_personal_law_communities, payer,
    moderate, generational, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, minority_personal_law_communities, beneficiary).

% Rights organizations and counsel who select which practices to challenge, frame the constitutional questions, and supply the petitioners. Winning petitions builds their reputations and funding, and their case selection determines which inequities reach the court and in what order. If the judicial channel closed, they could redirect effort to legislative campaigning or international treaty bodies.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, public_interest_litigators, beneficiary,
    organized, biographical, mobile, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority__judicial_harmonization_reading, public_interest_litigators, agenda_setter).

% Academic and comparative-law analysts who track the doctrinal trajectory across the personal codes, benchmark it against other plural legal systems, and publish assessments of whether the case-by-case pathway is converging, stalling, or being applied selectively. They bear none of the arrangement's costs and collect none of its gains.
narrative_ontology:constraint_stakeholder(marriage_authority__judicial_harmonization_reading, comparative_family_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority__judicial_harmonization_reading, supreme_court_judiciary).
narrative_ontology:fixing_cost_class(marriage_authority__judicial_harmonization_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a uniform constitutional floor of marital and familial rights across otherwise divergent religious personal law systems, resolving conflicts between code and constitution one controversy at a time in a field where the legislature will not act comprehensively; preserves the plurality of codes while guaranteeing minimum entitlements to everyone governed by them.
% TRANSFER_FUNCTION: Moves adjudicative authority over marriage norms from communal religious bodies to the constitutional judiciary; moves legal validity from traditionally sanctioned practices to constitutionally compliant ones; and accumulates precedent capital — the standing to decide future family-law controversies — at the apex court.
% ABSENT_VOICES: The national legislature, which formally holds the authorship role and would insist family law is a legislative domain, participates only reactively. Communal autonomists who want no state role in marriage at all, and secularists who want immediate comprehensive codification, are both outside the case-by-case conversation, which proceeds judgment by judgment without a forum where either wing could bargain over the endpoint.
% DISAPPEARANCE_RATIONALE: If the mechanism vanished overnight, personal law codes would revert to unreviewed communal administration, the rights established by prior rulings would sit in doctrinal limbo with conflicting lower-court lines, pressure for immediate comprehensive codification would surge from both secularist and majoritarian directions, and every pending petitioner would lose the only functioning remedy channel.
% FOUNDING_PROBLEM: Religious personal law codes inherited through colonial accommodation contain provisions that conflict with constitutional guarantees of equality and dignity; the legislature has been politically unable to enact a uniform civil code since the founding directive was written, leaving individuals inside communities without remedy against discriminatory marriage practices.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: national law commission consultation papers documenting persistent inter-code inequality, international treaty-body reviews of family-law discrimination, dissenting judicial opinions, and — decisively — the personal law boards' own testimony, which concedes the discriminatory provisions exist while disputing the remedy. No party to the dispute denies the founding conflict; they dispute only who may resolve it.
narrative_ontology:disappearance_verdict(marriage_authority__judicial_harmonization_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority__judicial_harmonization_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority__judicial_harmonization_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority__judicial_harmonization_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority__judicial_harmonization_reading, 0.52, 'stealth/ox-alpha', 'none', direct).

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
 *   Extraction is moderate (0.52 at interval end) because every ruling transfers adjudicative territory from communal bodies to the court and the court's institutional stake compounds with each precedent — yet the same rulings deliver enforceable rights that no other channel was delivering, so the arrangement is not extractive overall. Suppression (0.45) reflects binding precedent backed by an enforcement apparatus that hardened over the interval: early interventions were declaratory and reversible by ordinary statute, while later ones carry criminal sanctions and contempt-backed compliance. Theater is low-to-moderate (0.22): the core activity is functional rights adjudication, with a growing ceremonial component of uniformity rhetoric that outruns actual convergence. Accessibility collapse is low (0.30) because alternatives remain genuinely open — the legislature could codify tomorrow, communities can reinterpret internally, litigators can switch forums. Resistance is substantial (0.60): board-led campaigns, a historical statutory reversal of a landmark ruling, and recurring political mobilization. The temporal series runs on one shared six-point grid (T0-T40, mapping roughly 1985-2025) so every metric is authored at every examined time point; the suppression_requirement series is included because the story specifically tracks enforcement-capacity hardening, not merely shifting extraction. The interval also contains a visible cycle — landmark ruling, communal backlash, legislative retrenchment, renewed intervention — smoothed here into the monotonic trend; the underlying oscillation is documented in the rationale rather than fabricated into extra grid points.
 *
 * PERSPECTIVAL GAP:
 *   The payer and beneficiary seats compute differently from the same structure. From the communal authorities' position the mechanism is incremental dispossession: each ruling converts their interpretive monopoly into a constitutional question someone else decides. From the women's and litigators' seats the identical mechanism is the only functioning remedy in a field the legislature abandoned. The agenda-setter seat experiences it as obligation — a docket it cannot refuse without betraying its declared role. Same-level divergence is sharpest between the two institutional actors: the communal authorities and the national legislature hold comparable standing, yet the authorities are identity_locked (abandoning custodianship of the tradition would dissolve their own position) while the legislature is mobile (it could restructure the entire field by statute and chooses not to). Exit options, not power, differentiate these seats.
 *
 * DIRECTIONALITY LOGIC:
 *   Declared beneficiaries derive low directionality: the court (d near 0.15 — it bears docket load and legitimacy expenditure but nets authority), women under personal law (low d, dampened further by their trapped exit keeping them inside the arrangement's protection), and litigators (low d with career upside). Declared payers derive high directionality: communal authorities near the full-target end (identity_locked, nothing analogous to arbitrage), traditional adherents similarly high, and minority communities intermediate — they appear in the victims array but hold a genuine secondary beneficiary position, so their effective extraction sits below the pure-payer seats. The legislature is deliberately left undeclared in the beneficiary/victim arrays: its relationship is option-value (it can end the mechanism at will) rather than net flow, and no directionality override is used because overrides key on the power atom and would collide across the two differently-situated moderate agents. Scope is national throughout, so the engine's scope amplification applies uniformly.
 *
 * MANDATROPHY ANALYSIS:
 *   The scaffold claim is what keeps this story honest against both sibling misreadings. Read from the secularist seat, the mechanism looks like usurpation that blocks the proper legislative fix; read from the communal seat, it looks like pure taking. The scaffold classification holds both errors off by naming the genuine coordination function (a rights floor no one else will build) alongside the transitional justification (the mechanism consumes its own domain — every completed harmonization narrows the residual docket, and full convergence retires it). The has_sunset_clause declaration encodes that structural terminus, not a calendared one. The mandatrophy risk runs the other direction: if the court's institutional interest in a permanent harmonization docket outlives the convergence it was built to produce, the scaffold decays toward theatrical maintenance, and the R5 interview (status live x verdict world_rearranges) currently returns no zombie flag — the founding problem is corroborated as live by parties outside the beneficiary set, so the arrangement still earns its keep. The omega on sunset realism keeps that decay hypothesis testable rather than assumed.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_position_delta,
    'How would this arrangement''s classification change if authored under a sibling reading of the marriage_authority kernel instead of the judicial_harmonization_reading?',
    'Compare the sibling story files directly: the secularist_reading moves the epsilon referent to the legislative abdication the judicial stopgap enables; the communal_autonomy_reading makes the imposed floor itself the taking; the gender_rights_reading shifts beneficiary weight toward substantive outcomes over institutional process; the federalist_millet_reading recodes centralized floor-setting as the defeat of a consociational safeguard.',
    'The same historical record supports materially different epsilon values and types depending on which standing arrangement is under contest; cross-reading comparison is valid only at the level of the shared kernel, not the per-story metrics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_position_delta, conceptual, 'Committer-frame position: this story is one reading of the marriage_authority kernel, and sibling readings instantiate different constraints with different victim sets and epsilon referents.').

omega_variable(
    structural_sunset_realism,
    'Is the structural sunset real — does the mechanism actually terminate when convergence completes — or does the court''s institutional interest in a permanent harmonization docket convert the transitional arrangement into a self-perpetuating one?',
    'Track docket composition after major convergence milestones: if novel family-law controversies continue to be framed as constitutional-floor questions long after the codes have converged on settled doctrine, the termination condition is being deferred by institutional self-interest rather than by genuine residual conflict.',
    'If the sunset is rhetorical, the scaffold decays toward theatrical maintenance and the mandatrophy-resolved flag becomes operative; if real, the transitional classification holds and the mechanism should be assessed as successfully retiring itself.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(structural_sunset_realism, empirical, 'Whether the declared structural sunset clause corresponds to an actual termination condition or to indefinite institutional self-perpetuation.').

omega_variable(
    floor_enforcement_depth,
    'Are the rights established by harmonization rulings effective at the community level, or declaratory on paper while local practice continues unchanged?',
    'Post-ruling compliance studies comparing reported practice, registration data, and local dispute-resolution outcomes before and after landmark interventions, disaggregated by region and community.',
    'If enforcement is shallow, the women''s seat is a nominal rather than effective beneficiary and the arrangement''s coordination function is weaker than its doctrine suggests, raising measured extraction from the payers relative to delivered benefit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(floor_enforcement_depth, empirical, 'Depth of the constitutional floor: paper rights versus changed practice inside communities.').

omega_variable(
    selective_application_asymmetry,
    'Is the floor applied evenly across personal law codes, or does the pattern of intervention — frequent where a code was already statutorily reformed, sparse where it was not — reveal selective avoidance of the hardest cases?',
    'Doctrinal audit comparing intervention rates, remedial depth, and post-ruling follow-through across each personal law code over the full interval, controlling for litigation supply.',
    'Asymmetric application would indicate the mechanism manages conflict rather than converging the codes, supporting the ratchet reading and shifting classification weight away from a clean transitional account.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(selective_application_asymmetry, empirical, 'Even-handedness of constitutional floor imposition across the plural codes.').

omega_variable(
    stealth_ucc_ratchet,
    'Is case-by-case harmonization a deliberate ratchet toward de facto uniformity — the secularist endpoint reached by judicial means — or bounded floor-setting that respects permanent pluralism?',
    'Read the doctrinal trajectory''s asymptote: if the court''s stated principle is ''remove what violates rights, preserve the rest,'' pluralism is the design; if successive rulings treat residual inter-code difference itself as the defect, uniformity is the design.',
    'Resolving this determines which sibling reading the mechanism actually serves, and therefore which family edge carries the causal weight in contamination analysis.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(stealth_ucc_ratchet, conceptual, 'Endpoint ambiguity: convergence-toward-uniformity versus bounded rights floor over lasting pluralism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority__judicial_harmonization_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(marr_tr_t0, marriage_authority__judicial_harmonization_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(marr_tr_t8, marriage_authority__judicial_harmonization_reading, theater_ratio, 8, 0.12).
narrative_ontology:measurement(marr_tr_t16, marriage_authority__judicial_harmonization_reading, theater_ratio, 16, 0.15).
narrative_ontology:measurement(marr_tr_t24, marriage_authority__judicial_harmonization_reading, theater_ratio, 24, 0.17).
narrative_ontology:measurement(marr_tr_t32, marriage_authority__judicial_harmonization_reading, theater_ratio, 32, 0.2).
narrative_ontology:measurement(marr_tr_t40, marriage_authority__judicial_harmonization_reading, theater_ratio, 40, 0.22).

% Extraction over time
narrative_ontology:measurement(marr_be_t0, marriage_authority__judicial_harmonization_reading, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(marr_be_t8, marriage_authority__judicial_harmonization_reading, base_extractiveness, 8, 0.41).
narrative_ontology:measurement(marr_be_t16, marriage_authority__judicial_harmonization_reading, base_extractiveness, 16, 0.44).
narrative_ontology:measurement(marr_be_t24, marriage_authority__judicial_harmonization_reading, base_extractiveness, 24, 0.47).
narrative_ontology:measurement(marr_be_t32, marriage_authority__judicial_harmonization_reading, base_extractiveness, 32, 0.5).
narrative_ontology:measurement(marr_be_t40, marriage_authority__judicial_harmonization_reading, base_extractiveness, 40, 0.52).

% Suppression requirement over time
narrative_ontology:measurement(marr_su_t0, marriage_authority__judicial_harmonization_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(marr_su_t8, marriage_authority__judicial_harmonization_reading, suppression_requirement, 8, 0.27).
narrative_ontology:measurement(marr_su_t16, marriage_authority__judicial_harmonization_reading, suppression_requirement, 16, 0.3).
narrative_ontology:measurement(marr_su_t24, marriage_authority__judicial_harmonization_reading, suppression_requirement, 24, 0.34).
narrative_ontology:measurement(marr_su_t32, marriage_authority__judicial_harmonization_reading, suppression_requirement, 32, 0.4).
narrative_ontology:measurement(marr_su_t40, marriage_authority__judicial_harmonization_reading, suppression_requirement, 40, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority__judicial_harmonization_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, communal_autonomy_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, secularist_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, gender_rights_reading).
narrative_ontology:affects_constraint(marriage_authority__judicial_harmonization_reading, federalist_millet_reading).

% DUAL FORMULATION NOTE:
% Constraint family: the natural-language concept 'marriage authority in a plural legal order' decomposes into five kernel readings, each emitted as a separate story per the epsilon-invariance principle. This file instantiates judicial_harmonization_reading, whose epsilon referent is the case-by-case judicial floor mechanism itself; the sibling files author the same historical record under different standing arrangements (communal authorship, legislative authorship, equality-guarantee expansion, consociational fragmentation), yielding different epsilon values, beneficiary/victim structures, and types. Every successful harmonization ruling changes the operating environment of all four siblings — eroding communal authorship space, lowering legislative urgency, supplying the vehicle for equality litigation, and weakening the factual case for fragmentation as protection — which is why the edges run from this story to each sibling. The upstream/downstream structure is mechanism-to-claim: this reading supplies the institutional channel; the gender_rights_reading supplies much of the substantive docket that flows through it.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
