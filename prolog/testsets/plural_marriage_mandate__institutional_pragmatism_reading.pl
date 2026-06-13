% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-11
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
    narrative_ontology:boltzmann_floor_override/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: plural_marriage_mandate__institutional_pragmatism_reading
 *   human_readable: 1890 Manifesto: Institutional Survival via Doctrinal Legitimation
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto suspending plural marriage was orchestrated by the
 *   Church of Jesus Christ of Latter-day Saints' leadership under federal
 *   coercion (Edmunds-Tucker Act 1887). This constraint story instantiates
 *   the INSTITUTIONAL PRAGMATISM reading: the Manifesto is understood as
 *   strategic doctrinal legitimation deployed to secure institutional
 *   survival, not as authentic revelation of God's will. The core structural
 *   claim is that a real coordination function (surviving as an institution)
 *   was entangled with asymmetric extraction (costs borne by practicing
 *   polygamists, deceived monogamists, and compliant members, benefits reaped
 *   by leadership in the form of restored legal rights and institutional
 *   continuity). The constraint's persistence from 1890 onwards depends on
 *   the theater ratio remaining high — the appearance of doctrinal authority
 *   and revelation narrative conceals the actual mechanism: capitulation
 *   under coercive pressure. Secret continuations of plural marriage
 *   (1890–1904 and beyond) demonstrate that the practice was not doctrinally
 *   renounced, only publicly suspended while the legitimation narrative (the
 *   'revelation') provided cover for institutional survival.
 *
 * KEY AGENTS:
 *   - church_leadership: institutional agenda-setter, faces state suppression, orchestrates the Manifesto as survival mechanism, collects restored legal standing
 *   - practicing_polygamists: moderate-power payers, identity-locked (faith, families, doctrinal belief), coerced into secret-keeping or doctrinal betrayal
 *   - deceived_monogamists: powerless payers and excluded voices, trapped by property law and information asymmetry, bear relational costs of secret continuations
 *   - rank_and_file_members: powerless beneficiaries and payers, gain institutional survival but lose doctrinal coherence
 *   - federal_government: institutional observer, enforces Edmunds-Tucker Act, accepts the Manifesto as capitulation
 *   - anti_polygamy_reformers: organized observers, interpret the Manifesto as either victory or deception depending on whether secret continuations are known
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.79).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.71).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.79).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.71).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "1890 Manifesto: Institutional Survival via Doctrinal Legitimation").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, '67881e39-56a1-4220-8d08-dddab411a1e8').
narrative_ontology:cs_kernel_codification('67881e39-56a1-4220-8d08-dddab411a1e8', fixed_text).
narrative_ontology:cs_authority_grounding('67881e39-56a1-4220-8d08-dddab411a1e8', extraction).
narrative_ontology:cs_interpretation_layer_present('67881e39-56a1-4220-8d08-dddab411a1e8').
narrative_ontology:cs_reading_relation('67881e39-56a1-4220-8d08-dddab411a1e8', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('67881e39-56a1-4220-8d08-dddab411a1e8', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('67881e39-56a1-4220-8d08-dddab411a1e8', foundational, revelation_as_institutional_instrument).
narrative_ontology:cs_axiom_status(revelation_as_institutional_instrument, holdable).
narrative_ontology:cs_axiom_grounding('67881e39-56a1-4220-8d08-dddab411a1e8', revelation_as_institutional_instrument, instrumental).
narrative_ontology:cs_axiom('67881e39-56a1-4220-8d08-dddab411a1e8', foundational, doctrine_subordinate_to_institutional_survival).
narrative_ontology:cs_axiom_status(doctrine_subordinate_to_institutional_survival, holdable).
narrative_ontology:cs_axiom_grounding('67881e39-56a1-4220-8d08-dddab411a1e8', doctrine_subordinate_to_institutional_survival, empirically_contingent).
narrative_ontology:cs_reference_frame('67881e39-56a1-4220-8d08-dddab411a1e8', plural_marriage_as_eternal_doctrinal_requirement).
narrative_ontology:cs_drift_state('67881e39-56a1-4220-8d08-dddab411a1e8', post_1890_manifesto, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('67881e39-56a1-4220-8d08-dddab411a1e8', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, practicing_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_members).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_members).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, institutional_survival_doctrine).
narrative_ontology:constraint_vindicates(plural_marriage_mandate__institutional_pragmatism_reading, strategic_revelation_narrative).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces federal coercion via the Edmunds-Tucker Act (1887): loss of corporate charter, seizure of church property, political disenfranchisement, and criminal prosecution of polygamists. The leadership orchestrates the 1890 Manifesto as a doctrinal framework justifying public abandonment of plural marriage while maintaining institutional survival and eventual restoration of political rights. They frame the suspension as divine revelation rather than capitulation, collecting restored legitimacy and legal standing without explicitly renouncing the doctrine.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Have built lives, families, and faith identity around plural marriage as a sacred principle. The Manifesto coerces them to choose between abandoning spouses (and the doctrine they believe divine), facing prosecution and property loss, or continuing in secret while the church publicly disavows them. Many comply publicly while maintaining clandestine plural families through 1904 and beyond. The constraint operates by forcing them to bear the cost of institutional survival through personal theological and relational betrayal.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, practicing_polygamists, payer,
    moderate, biographical, identity_locked, regional).

% Women and children in monogamous family structures (or those who believed they entered monogamous unions) who discover after 1890 that their husbands maintained plural wives in secret. They are trapped by property law, social dependence, and lack of access to information about the covert continuations. The constraint extracts from them by rendering their family arrangements unstable and deceiving them about the institutional commitment to monogamy. They have no formal seat at the decision-making table.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    powerless, biographical, trapped, regional).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, excluded).

% Enforces the Edmunds-Tucker Act and subsequent anti-polygamy statutes. From their structural seat, the Manifesto represents success: the church capitulates under pressure. Accepting the Manifesto at face value, federal authorities withdraw enforcement and restore the church's corporate rights by the mid-1890s.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, observer,
    institutional, generational, analytical, national).

% Benefit from the church's institutional survival and restored political rights. They also bear the cost of the doctrinal shift: members who accepted plural marriage as sacred truth must now treat it as cancelled by divine will, without explicit explanation of the reversal. The constraint extracts from them through theological gaslighting (the doctrine did not change; God simply suspended it).
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_members, beneficiary,
    powerless, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_members, payer).

% Activists, journalists, and lawmakers (outside the church) who campaigned for the Edmunds-Tucker Act. From their seat, the Manifesto is either a genuine victory (if read as authentic doctrinal abandonment) or evidence of continued deception (if secret continuations are known). Their interpretive frame shapes whether the constraint is seen as resolved or as a sophisticated concealment mechanism.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, anti_polygamy_reformers, observer,
    organized, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the church's institutional structure, corporate legal standing, and political enfranchisement by creating a public doctrinal framework (the Manifesto) that satisfies federal coercion while internally maintaining operational control over compliance. The coordination problem solved: how to survive state suppression without explicit institutional dissolution or leadership capitulation.
% TRANSFER_FUNCTION: Transfers institutional survival costs from church leadership (would have faced ongoing property seizure, disenfranchisement, prosecution) to practicing polygamists (who lose public legitimacy, face secret-keeping costs, and relational disruption) and to deceived monogamists (who lose access to truthful information about family stability). The church's political and legal rights are transferred back to it by the federal government in exchange for the public Manifesto.
% ABSENT_VOICES: Practicing polygamists (especially women in plural marriages) and deceived monogamists are structurally excluded from the decision-making process that produced the Manifesto. They would object that they bore the costs of institutional survival without consent or knowledge. Rank-and-file members who had built faith identity around plural marriage as eternal doctrine would object to the theological reframing without explicit acknowledgment of reversal.
% DISAPPEARANCE_RATIONALE: If the 1890 Manifesto and its enforcement had not occurred, the church would have faced institutional dissolution or continued federal suppression; plural marriage would have remained openly practiced (and prosecuted) rather than moving underground; the deceived-monogamist harm (the gap between public doctrine and secret practice) would not have emerged as a structural feature. The constraint's absence would have meant either continued open polygamy and state persecution, or genuine institutional abandonment of the practice without the legitimation cover story.
% FOUNDING_PROBLEM: Federal government's escalating anti-polygamy enforcement (Edmunds Act 1882, Edmunds-Tucker Act 1887) threatened the church's survival as a legal institution: property seizure, corporate charter revocation, loss of voting rights, and personal prosecution of polygamists. The church faced institutional extinction or submission. The founding problem is the survival imperative under state coercion.
% FOUNDING_PROBLEM_CORROBORATION: Federal legislative records and judicial testimony from the 1880s document the intentionality of suppression and the church's institutional crisis. Church historical sources (later revealed and acknowledged by the institution itself) confirm the Manifesto was a tactical capitulation. Historical scholarship outside the church (Krakauer, Howe, Alexander, Compton) attests that the founding problem was institutional survival under coercion, not spontaneous doctrinal revelation. Federal authorities' own statements confirm they intended the Edmunds-Tucker Act to dissolve the church as a corporate entity.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).

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
 *   The extractiveness metric (0.68 at 1890, peaking at 0.71 in 1896) reflects that the constraint transfers institutional survival costs from leadership to subordinate stakeholders. It rises from 1880–1890 (federal pressure escalates) and peaks around 1896 (maximum suppression required to maintain the public/secret divide), then declines after 1904 (as continuations wind down and federal enforcement relaxes). Theater ratio is the key diagnostic: it spikes at 1890 (0.71) precisely when the Manifesto is issued, tracking the gap between doctrinal claim (revelation suspended plural marriage) and actual practice (continuations ongoing). Theater remains high through 1904 because the constraint's persistence depends on maintaining the appearance of compliance while secretly continuing. After 1904, as visible continuations cease and the constraint becomes straightforward doctrinal prohibition, theater ratio falls (to 0.52 by 1910). Suppression requirement mirrors theater: it peaks when the gap between public and secret is widest (1890–1896) and declines as the public/secret divide narrows. Resistance falls over the interval as practiced polygamists are replaced by a generation for whom plural marriage is historical doctrine, not lived practice. The coercion grid shows differential pressure across levels: structural coercion rises (federal law enforcement), organizational coercion rises (institutional enforcement of the Manifesto), class-level coercion increases but less dramatically (shared pressure on the polygamist cohort), and individual-level coercion is high and sustained (the personal cost of compliance or secret-keeping). Resistance begins highest at the structural and organizational levels (1880: federal government and church leadership openly contest) and falls as contestation formalizes into the Manifesto agreement. Individual and class resistance declines more slowly, reflecting the persistence of actual plural marriages and relational betrayals throughout the period.
 *
 * PERSPECTIVAL GAP:
 *   From the church leadership seat: the 1890 Manifesto is a divinely authorized reinterpretation that preserves the church's salvific mission and restores institutional viability. The constraint is experienced as a genuine coordination problem (institutional survival) with a revealed solution. From the practicing polygamist seat: the Manifesto is coercive capitulation cloaked in doctrinal language, forcing them to choose between relational dissolution and secret-keeping, while leadership escapes the costs. The constraint is experienced as extraction legitimized by false revelation. From the deceived monogamist seat: the Manifesto is a lie — it claims public doctrinal abandonment of plural marriage while the institution secretly tolerates and shields continuations, leaving deceived spouses without information. The constraint is experienced as suppression sustained by information control. From the rank-and-file member seat: the Manifesto is doctrine that fell from heaven, but its reversal (from eternal to suspended) without explicit acknowledgment is theologically destabilizing. The constraint is experienced as gaslighting. The engine computes a per-seat classification from these structural divergences; the authored claim (tangled_rope) names the structural fact — coordination entangled with asymmetric extraction — while the metrics describe the observable operation (high extraction, high theater, active enforcement).
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership is the agenda-setter with institutional power and constrained exit (they face institutional dissolution without a solution). Their directionality is near zero (full beneficiary): they initiate the Manifesto, collect restored legal rights, and bear minimal personal cost. Practicing polygamists are payers with identity-locked exit: they cannot leave without severing faith, family, and community identity. Their directionality approaches 1.0 (full target): they bear the extraction (relational dissolution or secret-keeping) and generate no visible benefit to themselves — the benefit accrues to leadership. Deceived monogamists are payers with trapped exit: they have no information about the true constraint and no legal recourse. Their directionality is at 1.0 (full target). Rank-and-file members are dual-positioned: they benefit from institutional survival (beneficiary role) but bear the cost of theological incoherence (payer role). Their directionality sits near 0.5 (symmetric), though the benefits are collective and diffuse while the costs are personal and immediate. Federal government observers sit at analytical directionality (0.5 by convention). Anti-polygamy reformers similarly observe, with variable perception based on whether they are deceived (Manifesto as victory, d~0.2 toward beneficiary) or informed (Manifesto as concealment, d~0.7 toward target).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint presents a classic mandatrophy pattern: the 1890 Manifesto's mandate was institutional survival under federal coercion. That mandate remains 'live' (institutional survival is always present), but the founding problem — federal suppression of polygamy — has shifted from state of crisis (1887) to resolved/institutionalized state (post-1904). The constraint persists (and extractiveness remains elevated at 0.58 even by 1910) despite the founding problem's resolution because the legitimation mechanism (the revelation narrative) has calcified into doctrine. The theater ratio's persistence (0.52 by 1910, still well above the functional floor) indicates that performative maintenance is ongoing — the constraint is maintained partly through narrative preservation rather than active enforcement of the original survival imperative. A genuine mandatrophy crack appears at the institutional level: by the second and third generation after 1890, the Manifesto is invoked as eternal doctrine, not as tactical capitulation, producing theological absurdity (God revealed the suspension, not God was coerced). The constraint does not cleanly resolve because the M-set gap (doctrine vs. practice, mandate vs. function) is institutionalized: speaking the truth about the origin — that it was coerced capitulation — would delegitimize the institution's entire revelatory claim. Thus the constraint persists as inert ideology rather than active mechanism, supported by the theater ratio and suppression_requirement maintaining the gap between official narrative and historical fact.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legitimation_vs_revelation,
    'Is the 1890 Manifesto a genuine divine revelation restructuring the cosmos, or is the revelation narrative a post-hoc legitimation constructed to conceal institutional capitulation?',
    'Comparison of contemporaneous internal church communications (diaries, private letters, leadership meeting minutes pre-1890 and post-1890) with the public revelation narrative; analysis of whether leadership privately anticipated federal suppression and the Manifesto as response, or whether the revelation comes unexpectedly. Examination of whether the revelation narrative shows marks of ex-post construction (retrofitting events to narrative) or authentic prophetic foresight.',
    'If the revelation narrative is post-hoc legitimation, the constraint''s classification as tangled_rope (coordination + extraction) holds; if the revelation is authentic (even in the eyes of the believers), the constraint shifts toward the endogenous_reinterpretation_reading and would be classified differently. The extraction component depends on whether the narrative is strategic (pragmatism reading: ε rises) or genuine (reinterpretation reading: ε falls as the transfer becomes spiritual rather than institutional).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimation_vs_revelation, empirical, 'Whether the revelation narrative is legitimation strategy or authentic prophecy.').

omega_variable(
    identity_lock_mechanism,
    'For practicing polygamists, is their suppression of continued practice structural (economic dependency on the church, legal vulnerability, property held in trust) or internalized (identity fusion with faith and institutional belonging)?',
    'Post-suppression cohort analysis: tracking the behavioral trajectories of practicing polygamists who left the church after 1890 (or were excommunicated) versus those who remained secret. If suppression is primarily structural, those who exit the structural context (leave the church, relocate outside reach) should show higher continued practice rates. If suppression is primarily internalized, exit from structure should not resolve it — identity-fused individuals carry suppression with them.',
    'If suppression is primarily structural, the constraint''s effective suppression score (0.79) is accurate to the situation. If substantially internalized, the effective suppression is higher (the constraint''s hold persists after structural exit), suggesting deeper extraction and identity capture than the structural measure alone indicates. This affects whether the constraint should be reclassified as identity_coordination (with higher complexity floor) or remains purely coercive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism, empirical, 'Structural vs. internalized suppression in polygamist identity-lock.').

omega_variable(
    secret_continuation_scope,
    'How many practicing polygamists continued plural marriages in secret after 1890, and for how long (1890–1904 only, or 1890–1940+)?',
    'Genealogical and demographic analysis of plural marriages documented post-1890 among church members; cross-referencing with excommunication records, schism records (Fundamentalist LDS Church formation 1913), and community oral histories. Establishing the scale and duration of the gap between public doctrine and secret practice.',
    'The larger and more sustained the secret continuations, the higher the theater_ratio (the constraint persists by maintaining the public/secret divide). If continuations were minimal and brief (1890–1900 only), theater_ratio should decline faster. If continuations were substantial and persistent (1890–1940+), the theater ratio remains high and the constraint''s legitimation requirement stays elevated longer. This affects the historical dating of when the constraint genuinely shifted from coercive suppression to normalized doctrine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(secret_continuation_scope, empirical, 'Extent and duration of secret plural marriage continuations post-1890.').

omega_variable(
    kernel_foreclosure_test,
    'Does the institutional pragmatism reading logically foreclose the endogenous reinterpretation reading, or can both coexist within different institutional actors'' frameworks?',
    'Examining whether church leadership could simultaneously hold (1) the public commitment to the revelation narrative and (2) the private knowledge that the revelation served institutional survival without the revelation''s truth-claim being false. Can a believer simultaneously hold that God acted AND that God acted under human coercion?',
    'If the readings foreclose each other, one reading''s core premise directly contradicts the other (e.g., revelation-as-legitimation rules out revelation-as-authentic). If they coexist, the same fact (the 1890 Manifesto) is interpreted differently by different constituencies without logical contradiction — the revelation could be authentic and strategically useful for survival. The reading_relations declaration depends on this resolution.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_foreclosure_test, conceptual, 'Logical compatibility of the institutional pragmatism and endogenous reinterpretation readings of the same kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1880, 1910).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1880, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1880, 0.22).
narrative_ontology:measurement_basis(plur_tr_t1880, observed).
narrative_ontology:measurement(plur_tr_t1887, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1887, 0.48).
narrative_ontology:measurement_basis(plur_tr_t1887, observed).
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.71).
narrative_ontology:measurement_basis(plur_tr_t1890, observed).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.74).
narrative_ontology:measurement_basis(plur_tr_t1896, observed).
narrative_ontology:measurement(plur_tr_t1904, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1904, 0.68).
narrative_ontology:measurement_basis(plur_tr_t1904, observed).
narrative_ontology:measurement(plur_tr_t1910, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1910, 0.52).
narrative_ontology:measurement_basis(plur_tr_t1910, observed).

% Extraction over time
narrative_ontology:measurement(plur_be_t1880, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1880, 0.35).
narrative_ontology:measurement_basis(plur_be_t1880, observed).
narrative_ontology:measurement(plur_be_t1887, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1887, 0.62).
narrative_ontology:measurement_basis(plur_be_t1887, observed).
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.68).
narrative_ontology:measurement_basis(plur_be_t1890, observed).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.71).
narrative_ontology:measurement_basis(plur_be_t1896, observed).
narrative_ontology:measurement(plur_be_t1904, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1904, 0.65).
narrative_ontology:measurement_basis(plur_be_t1904, observed).
narrative_ontology:measurement(plur_be_t1910, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1910, 0.58).
narrative_ontology:measurement_basis(plur_be_t1910, observed).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1880, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1880, 0.42).
narrative_ontology:measurement_basis(plur_su_t1880, observed).
narrative_ontology:measurement(plur_su_t1887, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1887, 0.71).
narrative_ontology:measurement_basis(plur_su_t1887, observed).
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.79).
narrative_ontology:measurement_basis(plur_su_t1890, observed).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.81).
narrative_ontology:measurement_basis(plur_su_t1896, observed).
narrative_ontology:measurement(plur_su_t1904, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1904, 0.77).
narrative_ontology:measurement_basis(plur_su_t1904, observed).
narrative_ontology:measurement(plur_su_t1910, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1910, 0.68).
narrative_ontology:measurement_basis(plur_su_t1910, observed).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=1880, tn=1910
narrative_ontology:measurement(plur_grid_01, plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse(class), 1880, 0.52).
narrative_ontology:measurement(plur_grid_02, plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse(class), 1910, 0.64).
narrative_ontology:measurement(plur_grid_03, plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse(individual), 1880, 0.41).
narrative_ontology:measurement(plur_grid_04, plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse(individual), 1910, 0.59).
narrative_ontology:measurement(plur_grid_05, plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse(organizational), 1880, 0.45).
narrative_ontology:measurement(plur_grid_06, plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse(organizational), 1910, 0.72).
narrative_ontology:measurement(plur_grid_07, plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse(structural), 1880, 0.38).
narrative_ontology:measurement(plur_grid_08, plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse(structural), 1910, 0.71).
narrative_ontology:measurement(plur_grid_09, plural_marriage_mandate__institutional_pragmatism_reading, resistance(class), 1880, 0.58).
narrative_ontology:measurement(plur_grid_10, plural_marriage_mandate__institutional_pragmatism_reading, resistance(class), 1910, 0.42).
narrative_ontology:measurement(plur_grid_11, plural_marriage_mandate__institutional_pragmatism_reading, resistance(individual), 1880, 0.54).
narrative_ontology:measurement(plur_grid_12, plural_marriage_mandate__institutional_pragmatism_reading, resistance(individual), 1910, 0.38).
narrative_ontology:measurement(plur_grid_13, plural_marriage_mandate__institutional_pragmatism_reading, resistance(organizational), 1880, 0.62).
narrative_ontology:measurement(plur_grid_14, plural_marriage_mandate__institutional_pragmatism_reading, resistance(organizational), 1910, 0.28).
narrative_ontology:measurement(plur_grid_15, plural_marriage_mandate__institutional_pragmatism_reading, resistance(structural), 1880, 0.48).
narrative_ontology:measurement(plur_grid_16, plural_marriage_mandate__institutional_pragmatism_reading, resistance(structural), 1910, 0.31).
narrative_ontology:measurement(plur_grid_17, plural_marriage_mandate__institutional_pragmatism_reading, stakes_inflation(class), 1880, 0.48).
narrative_ontology:measurement(plur_grid_18, plural_marriage_mandate__institutional_pragmatism_reading, stakes_inflation(class), 1910, 0.62).
narrative_ontology:measurement(plur_grid_19, plural_marriage_mandate__institutional_pragmatism_reading, stakes_inflation(individual), 1880, 0.61).
narrative_ontology:measurement(plur_grid_20, plural_marriage_mandate__institutional_pragmatism_reading, stakes_inflation(individual), 1910, 0.78).
narrative_ontology:measurement(plur_grid_21, plural_marriage_mandate__institutional_pragmatism_reading, stakes_inflation(organizational), 1880, 0.52).
narrative_ontology:measurement(plur_grid_22, plural_marriage_mandate__institutional_pragmatism_reading, stakes_inflation(organizational), 1910, 0.76).
narrative_ontology:measurement(plur_grid_23, plural_marriage_mandate__institutional_pragmatism_reading, stakes_inflation(structural), 1880, 0.35).
narrative_ontology:measurement(plur_grid_24, plural_marriage_mandate__institutional_pragmatism_reading, stakes_inflation(structural), 1910, 0.68).
narrative_ontology:measurement(plur_grid_25, plural_marriage_mandate__institutional_pragmatism_reading, suppression(class), 1880, 0.44).
narrative_ontology:measurement(plur_grid_26, plural_marriage_mandate__institutional_pragmatism_reading, suppression(class), 1910, 0.68).
narrative_ontology:measurement(plur_grid_27, plural_marriage_mandate__institutional_pragmatism_reading, suppression(individual), 1880, 0.51).
narrative_ontology:measurement(plur_grid_28, plural_marriage_mandate__institutional_pragmatism_reading, suppression(individual), 1910, 0.74).
narrative_ontology:measurement(plur_grid_29, plural_marriage_mandate__institutional_pragmatism_reading, suppression(organizational), 1880, 0.46).
narrative_ontology:measurement(plur_grid_30, plural_marriage_mandate__institutional_pragmatism_reading, suppression(organizational), 1910, 0.79).
narrative_ontology:measurement(plur_grid_31, plural_marriage_mandate__institutional_pragmatism_reading, suppression(structural), 1880, 0.38).
narrative_ontology:measurement(plur_grid_32, plural_marriage_mandate__institutional_pragmatism_reading, suppression(structural), 1910, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(plural_marriage_mandate__institutional_pragmatism_reading, 0.12).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel is instantiated by three structurally distinct readings: (1) institutional_pragmatism_reading (this constraint) — the Manifesto legitimizes survival-driven capitulation via revelation narrative; (2) endogenous_reinterpretation_reading — the Manifesto represents authentic prophetic reinterpretation; (3) exogenous_override_reading — the Manifesto represents federal coercion negating a divine requirement. Each reading has a distinct ε (extractiveness), beneficiary structure, and classification. The institutional pragmatism reading carries elevated extraction (0.68) because the beneficiary set (church leadership) collects institutional survival while the costs (relational betrayal, information suppression, theological gaslighting) accrue to victims (polygamists, deceived monogamists). The endogenous reinterpretation reading would show lower extraction because the beneficiary (institutional mission preservation) is spiritual rather than institutional-power-accumulation. The exogenous override reading would name federal suppressors as beneficiaries and the church as victim, producing a snare rather than tangled_rope. The readings are not alternative measurements of the same constraint — they are different constraints grounded in the same historical kernel. The ε-invariance principle requires decomposition: a single ε-value cannot accommodate the causal-narrative divergence across readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(plural_marriage_mandate__institutional_pragmatism_reading, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
