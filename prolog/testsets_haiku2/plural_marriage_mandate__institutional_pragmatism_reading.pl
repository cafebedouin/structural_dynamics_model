% ============================================================================
% CONSTRAINT STORY: plural_marriage_mandate__institutional_pragmatism_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   human_readable: Plural Marriage Mandate: Institutional Pragmatism Reading (1890 Manifesto as Strategic Capitulation)
 *   domain: religious_institutional_history/political_theology
 *
 * SUMMARY:
 *   The 1890 Manifesto issued by the Church of Jesus Christ of Latter-day
 *   Saints represents an institutional response to federal coercion targeting
 *   plural marriage. Under the institutional-pragmatism reading, the
 *   Manifesto is analyzed as a strategic adaptation in which leadership
 *   issues a public doctrinal suspension (framed as divine revelation) while
 *   covertly maintaining and directing plural marriage among select believers
 *   through 1904. The constraint operating here is the entanglement of
 *   institutional survival mechanisms (public renunciation to placate federal
 *   pressure) with doctrinal legitimation narratives (the claim of prophetic
 *   revelation as the ground of suspension). This reading treats the M-set
 *   gap (doctrine nominally unchanged, practice suspended, secret
 *   continuations) as the primary empirical observable, and asks whether the
 *   doctrinal framing is authentic reinterpretation or strategic cover for
 *   institutional capitulation. The beneficiaries are church leadership
 *   (restored political rights, institutional survival, continued internal
 *   authority). The victims are active polygamists (legal vulnerability,
 *   family separation, identity rupture) and deceived monogamists
 *   (retroactive betrayal of informed consent). This is distinct from the
 *   sibling readings: the endogenous-reinterpretation reading treats the
 *   Manifesto as genuine prophetic reinterpretation within the church's own
 *   theological framework; the exogenous-override reading treats it as
 *   coerced abandonment of a divine requirement, not reinterpretation. The
 *   institutional-pragmatism reading rejects both alternatives and locates
 *   the constraint in the structural entanglement of strategic capitulation
 *   and doctrinal legitimacy claims.
 *
 * KEY AGENTS:
 *   - Church leadership: institutional agenda-setter facing federal prosecution and property seizure; benefits from restored political rights and institutional survival through the Manifesto's public acceptance
 *   - Active polygamists: coerced to renounce the practice publicly while continuing covertly under leadership direction; bear legal vulnerability and identity rupture
 *   - Deceived monogamists: accept the Manifesto as genuine reinterpretation and adjust expectations; later discover secret continuations and institutional betrayal
 *   - Federal government: applied coercive pressure; excluded from knowledge of covert arrangements; accepts the Manifesto as resolving the conflict
 *   - Rank-and-file members: observe the institutional upheaval and doctrinal change; depend on leadership authority for interpretation
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(plural_marriage_mandate__institutional_pragmatism_reading, 0.72).
domain_priors:suppression_score(plural_marriage_mandate__institutional_pragmatism_reading, 0.78).
domain_priors:theater_ratio(plural_marriage_mandate__institutional_pragmatism_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(plural_marriage_mandate__institutional_pragmatism_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(plural_marriage_mandate__institutional_pragmatism_reading, tangled_rope).
narrative_ontology:human_readable(plural_marriage_mandate__institutional_pragmatism_reading, "Plural Marriage Mandate: Institutional Pragmatism Reading (1890 Manifesto as Strategic Capitulation)").
narrative_ontology:topic_domain(plural_marriage_mandate__institutional_pragmatism_reading, "religious_institutional_history/political_theology").

domain_priors:requires_active_enforcement(plural_marriage_mandate__institutional_pragmatism_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(plural_marriage_mandate__institutional_pragmatism_reading, 'aaa91958-25aa-4514-aef5-5b10a725a973').
narrative_ontology:cs_kernel_codification('aaa91958-25aa-4514-aef5-5b10a725a973', fixed_text).
narrative_ontology:cs_authority_grounding('aaa91958-25aa-4514-aef5-5b10a725a973', extraction).
narrative_ontology:cs_interpretation_layer_present('aaa91958-25aa-4514-aef5-5b10a725a973').
narrative_ontology:cs_reading_relation('aaa91958-25aa-4514-aef5-5b10a725a973', plural_marriage_mandate__endogenous_reinterpretation_reading, coexists_with).
narrative_ontology:cs_reading_relation('aaa91958-25aa-4514-aef5-5b10a725a973', plural_marriage_mandate__exogenous_override_reading, coexists_with).
narrative_ontology:cs_axiom('aaa91958-25aa-4514-aef5-5b10a725a973', foundational, doctrinal_claims_serve_institutional_survival).
narrative_ontology:cs_axiom_status(doctrinal_claims_serve_institutional_survival, holdable).
narrative_ontology:cs_axiom_grounding('aaa91958-25aa-4514-aef5-5b10a725a973', doctrinal_claims_serve_institutional_survival, empirically_contingent).
narrative_ontology:cs_axiom('aaa91958-25aa-4514-aef5-5b10a725a973', foundational, federal_coercion_primary_causal_driver).
narrative_ontology:cs_axiom_status(federal_coercion_primary_causal_driver, holdable).
narrative_ontology:cs_axiom_grounding('aaa91958-25aa-4514-aef5-5b10a725a973', federal_coercion_primary_causal_driver, empirically_contingent).
narrative_ontology:cs_reference_frame('aaa91958-25aa-4514-aef5-5b10a725a973', institutional_survival_through_doctrinal_legitimation).
narrative_ontology:cs_drift_state('aaa91958-25aa-4514-aef5-5b10a725a973', id_1920_federal_acceptance, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('aaa91958-25aa-4514-aef5-5b10a725a973', '').
narrative_ontology:cs_kernel_id(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, active_polygamists).
narrative_ontology:constraint_victim(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Faces existential institutional threat from federal prosecution, property seizure, and political disenfranchisement. Issues the 1890 Manifesto as a public renunciation of plural marriage while maintaining secret continuations and covert networks through 1904. Benefits from restored political rights, exemption from prosecution, and institutional survival. Controls the narrative frame (revelation, divine will) through which the suspension is publicly legitimated.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership, agenda_setter,
    institutional, generational, constrained, national).

% Face coercion to abandon plural marriage publicly while some continue covertly under institutional direction. Bear the costs of legal vulnerability (prosecution if discovered), family separation (some wives and children abandoned or hidden), identity rupture (core doctrinal commitment declared suspended), and institutional deception (leadership publicly denies what it secretly coordinates). Their religious identity and institutional belonging are fused with the practice.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, active_polygamists, payer,
    moderate, biographical, identity_locked, national).

% Accept the Manifesto as genuine doctrinal reinterpretation and adjust their family, financial, and institutional expectations accordingly. Subsequently discover that leadership coordinated secret plural continuations, rendering their consent retroactively misinformed. Bear the cost of institutional betrayal and loss of doctrinal coherence.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, deceived_monogamists, payer,
    moderate, biographical, constrained, national).

% Applied coercive pressure (prosecution, property seizure, disenfranchisement) to force abandonment of plural marriage. The Manifesto is formally accepted as resolving the legal conflict, though enforcement gaps (secret continuations) exist and are discovered decades later. Government authorities are excluded from knowledge of covert arrangements and would dispute the institutional-pragmatism reading as a characterization of the constraint.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, federal_government, excluded,
    institutional, generational, analytical, national).

% Accept institutional leadership's authority to interpret doctrine and policy. Most have no direct involvement in plural marriage but experience the institutional disruption, the claimed doctrinal reinterpretation, and the gradual disclosure of secret continuations. Their observational position is constrained by dependence on institutional belonging and information.
narrative_ontology:constraint_stakeholder(plural_marriage_mandate__institutional_pragmatism_reading, rank_and_file_members, observer,
    powerless, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(plural_marriage_mandate__institutional_pragmatism_reading, church_leadership).
narrative_ontology:fixing_cost_class(plural_marriage_mandate__institutional_pragmatism_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserves the institutional survival of a theocratic organization facing existential federal pressure by publicly renouncing a central doctrinal claim while covertly maintaining and directing plural marriage among select believers. Coordinates internal institutional loyalty (among rank-and-file and leadership) with external compliance (federal law and political restoration).
% TRANSFER_FUNCTION: Transfers institutional legitimacy and political restoration (exemption from prosecution, restoration of property rights and voting) from the federal government to church leadership, in exchange for public doctrinal suspension. Transfers legal vulnerability from institutional leadership to active polygamists, who continue the practice covertly under leadership direction and bear the prosecution risk.
% ABSENT_VOICES: Covert polygamists and their families are excluded from the formal institutional deliberation about the Manifesto's theological status; they experience the constraint in isolation, under leadership direction, without public voice. Federal prosecutors who would challenge the sincerity of the Manifesto are also excluded from institutional deliberation. Women in plural marriages face particular exclusion from deliberative power over their own family arrangements.
% DISAPPEARANCE_RATIONALE: If the constraint (the Manifesto as strategic adaptation) had not been authored, the church faced institutional dissolution: the federal government would have completed the disenfranchisement, property seizure, and prosecution it had begun. The church's institutional form, political standing, and territorial foothold in Utah would have reorganized radically. The federal government would have faced continued resistance and institutional defiance rather than accepted capitulation. The institutional landscape of the American West would have been substantially different.
% FOUNDING_PROBLEM: Federal coercion targeting plural marriage practice threatened the survival of the institution, the political and legal standing of its members, and the continuity of its theological commitment. The institution needed to end federal pressure (through apparent doctrinal reinterpretation) while preserving internal doctrinal coherence and institutional loyalty among core believers, managing the contradiction between public renunciation and private continuance.
% FOUNDING_PROBLEM_CORROBORATION: Historians (Kathryn M. Daynes, Sarah Barringer Gordon, Joan Smyth Iversen, David Bigler) document the federal coercive pressure in contemporaneous legislative records, court filings, and institutional archives. The federal government's own correspondence and policy documents attest to the coercive pressure and the motivation to force abandonment of plural marriage. Church historians (Michael Quinn, D. Michael Quinn, Andrew Jenson) document the secret continuations of plural marriage 1890-1904 in archival research and official church records. Fundamentalist groups that broke away specifically to maintain plural marriage provide external corroboration that the founding problem (institutional conflict between doctrinal commitment and federal coercion) was perceived as genuine by at least some institutional members. Post-1920 church admissions and archival releases corroborate the covert continuations.
narrative_ontology:disappearance_verdict(plural_marriage_mandate__institutional_pragmatism_reading, world_rearranges).
narrative_ontology:founding_problem_status(plural_marriage_mandate__institutional_pragmatism_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku2', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(plural_marriage_mandate__institutional_pragmatism_reading, 'none', 1).
narrative_ontology:epsilon_provenance(plural_marriage_mandate__institutional_pragmatism_reading, 0.72, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.58 (1890, immediate post-Manifesto, compliance unclear) to 0.74 (1902, peak of covert coordination during second presidency of Lorenzo Snow and Brigham Young Jr., maximum M-set gap between public doctrine and private practice) then declines (1908-1920, as federal acceptance solidifies and covert operations wind down). Theater ratio rises from 0.52 to 0.72 over the same period: the constraint's functional power decreases as institutional survival is secured, but the narrative maintenance (public commitment to the Manifesto despite secret continuations) increases. Suppression requirement peaks at 0.82 (1902) when covert polygamists are most actively managed and hidden, then declines as the practice is genuinely abandoned. The measurement series captures the lifecycle of the constraint: initial accommodation (1890), deepened deception (1896-1902), gradual normalization (1908-1920). All metrics share the same time grid (1890, 1896, 1902, 1908, 1914, 1920) so temporal analysis has aligned data.
 *
 * PERSPECTIVAL GAP:
 *   The institutional-pragmatism reading predicts radically different seat classifications. From the agenda-setter seat (church leadership): the Manifesto is a legitimate institutional survival mechanism and negotiated accommodation with federal power; the constraint appears as rope (coordinating internal unity while navigating external pressure). From the victim seats (active polygamists, deceived monogamists): the same constraint appears as tangled rope or snare — institutional survival is secured at the cost of coerced practice-suspension and systematic deception. The engine computes these divergences from the authored structural data (power, exit, beneficiary/victim status). The pragmatism reading asserts that both seats are perceiving the same constraint structure; their divergent type-classifications reflect their divergent structural positions (beneficiary vs. victim), not different constraints or mere disagreement.
 *
 * DIRECTIONALITY LOGIC:
 *   Church leadership are structural beneficiaries: they set the constraint (control the Manifesto narrative), extract institutional survival and restored rights, and face low exit costs (they authored the adaptation and control the institutional apparatus). Directionality d near 0.0-0.2 (low extraction, high subsidy of institutional security). Active polygamists are trapped victims: identity-locked by religious commitment and institutional belonging, coerced to renounce publicly while continuing covertly under surveillance and leadership direction, bearing prosecution risk. Directionality d near 0.85-0.95 (high extraction). Deceived monogamists are partial victims: they bear the cost of betrayed consent (institutional deception about the authenticity of the reinterpretation) and loss of doctrinal coherence, but they benefit from institutional stability and their own legal security (they are not prosecuted). Directionality d near 0.55-0.70 (moderate extraction, some institutional benefit). Federal government is excluded: they are not in the relational structure of the constraint itself (they set the coercive background condition but do not participate in the internal institutional mechanism).
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (institutional survival under federal pressure) remains live throughout the interval: the federal threat does not lift, only the mode of institutional response shifts from public plural marriage to covert practice. However, the mandatrophy analysis shows that the pragmatism reading addresses the founding problem differently than the sibling readings. The endogenous-reinterpretation reading claims the Manifesto resolves the founding problem by a genuine doctrinal update (God revealed the suspension). The pragmatism reading claims the Manifesto *manages* the founding problem by strategic adaptation: it does not resolve the underlying conflict (doctrine vs. coercion) but rather suspends it through deception and selective practice. The founding-problem status is 'live' because the institutional conflict never disappears; it is merely displaced into the M-set gap (doctrine unchanged, practice suspended, secret continuations). Mandatrophy would arise if the constraint persisted after its founding problem had been solved — if the federal threat had lifted and the church continued public plural-marriage renunciation without institutional benefit. The pragmatism reading predicts that the constraint should weaken and eventually dissolve once institutional survival is secure (which matches the measurement trajectory: extraction and theater decline 1908-1920 as federal acceptance hardens and covert operations wind down). The constraint's persistence through the post-1920 era (genuine plural-marriage abandonment) would then represent mandatrophy: the Manifesto and its doctrinal legitimation persisting as institutional inertia after the founding problem (federal coercion) has been resolved.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    doctrine_vs_strategy_boundary,
    'Is the 1890 Manifesto a genuine doctrinal reinterpretation (the doctrine was authoritatively updated by divine revelation), or a strategic institutional adaptation (the doctrinal narrative is constructed to legitimize capitulation)?',
    'Content analysis of the Manifesto''s theological framing compared to the secret continuations documented in archival records (Fundamentalist Historical Library, church internal documents, missionary correspondence). Compare the doctrinal arguments offered in the Manifesto to the theological justifications given to covert practitioners — if they diverge, the readings are addressing different constraints; if they align, the constraint is unified.',
    'If reinterpretation, the endogenous reading predominates and the pragmatism reading becomes a skeptical external reading rather than a structural classification. If strategy, the pragmatism reading correctly locates the constraint in the entanglement of survival mechanism and doctrinal legitimation. This directly affects whether the constraint should be classified as rope (institutional coordination solving a genuine theological problem) or tangled_rope (institutional survival entangled with doctrinal cover).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrine_vs_strategy_boundary, conceptual, 'Whether the doctrinal reinterpretation is theologically authentic or strategically constructed to legitimize capitulation.').

omega_variable(
    intentionality_of_deception,
    'Did church leadership knowingly maintain and direct plural marriage in violation of the Manifesto, or did covert continuations arise as grassroots defiance uncoordinated with leadership?',
    'Archival evidence of leadership authorization and direction of covert marriages (particularly the private correspondence and administrative records of Brigham Young Jr., Joseph F. Smith, and the First Presidency 1890-1904). Evidence of leadership knowledge, resource allocation, or strategic coordination distinguishes institutional deception from rank-and-file violation.',
    'If coordination was grassroots, the constraint may be better modeled as snare (leadership extraction through institutional control) rather than tangled_rope (entanglement of institutional survival and doctrinal legitimation). If coordination was leadership-directed, the pragmatism reading''s model of strategic adaptation is supported and tangled_rope classification is appropriate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(intentionality_of_deception, empirical, 'Whether covert plural marriage was leadership-directed institutional deception or uncoordinated rank-and-file resistance.').

omega_variable(
    reading_foreclosure_boundary,
    'Do the endogenous-reinterpretation reading and the exogenous-override reading logically foreclose each other, or do they coexist as rival positions held by different parties?',
    'Examine whether defenders of the endogenous reading (church theologians, historians within the tradition) hold the exogenous reading to be self-contradictory or merely wrong. Compare to actual historical positions: exogenous defenders (Fundamentalist breakaway groups, some historical critics) maintain the exogenous reading as a live alternative, not as logically foreclosed by endogenous commitment.',
    'If the readings coexist (neither forecloses the other), they should be linked as ''coexists_with'' in the reading_relations. If one forecloses the other, the relation should be ''forecloses''. This determines the structure of the kernel contest: are the readings competing truth-claims (coexist) or logically exclusive alternatives (forecloses)?',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reading_foreclosure_boundary, conceptual, 'Whether the endogenous and exogenous readings logically foreclose each other or coexist as rival positions.').

omega_variable(
    victim_set_identity_fusion,
    'For active polygamists classified as identity-locked, what is the mechanism of identity fusion: professional/institutional identity (the role of ''polygamist'' in church hierarchy), relational identity (self-concept constituted through plural family), or ideological identity (worldview where plural marriage is divinely mandated)?',
    'Comparative analysis of Fundamentalist and mainstream-church polygamists 1904-onward: did Fundamentalists leave the church specifically to maintain plural marriage (evidence for ideological identity), did they remain in plural-family structures even after institutional defection (relational identity), or did they seek alternative institutional frameworks where plural marriage conferred status (professional identity)? Exit narratives from historical accounts provide evidence.',
    'If identity fusion is primarily ideological, breaking the identity-lock requires theological reorientation — the exit cost is doctrinal coherence. If relational, breaking the lock requires family dissolution — the exit cost is kinship rupture. If professional, it requires status loss — the exit cost is institutional standing. The classification of identity-locked exit is correct in all cases, but the suppression mechanism differs (internalized belief vs. structural relationship vs. status dependency), affecting how the constraint''s suppression would respond to changed circumstances.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(victim_set_identity_fusion, conceptual, 'What mechanism binds the identity-locked victims to the constraint: ideological, relational, or professional identity.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(plural_marriage_mandate__institutional_pragmatism_reading, 1890, 1920).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(plur_tr_t1890, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1890, 0.52).
narrative_ontology:measurement(plur_tr_t1896, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1896, 0.65).
narrative_ontology:measurement(plur_tr_t1902, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1902, 0.72).
narrative_ontology:measurement(plur_tr_t1908, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1908, 0.71).
narrative_ontology:measurement(plur_tr_t1914, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1914, 0.68).
narrative_ontology:measurement(plur_tr_t1920, plural_marriage_mandate__institutional_pragmatism_reading, theater_ratio, 1920, 0.62).

% Extraction over time
narrative_ontology:measurement(plur_be_t1890, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1890, 0.58).
narrative_ontology:measurement(plur_be_t1896, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1896, 0.68).
narrative_ontology:measurement(plur_be_t1902, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1902, 0.74).
narrative_ontology:measurement(plur_be_t1908, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1908, 0.72).
narrative_ontology:measurement(plur_be_t1914, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1914, 0.68).
narrative_ontology:measurement(plur_be_t1920, plural_marriage_mandate__institutional_pragmatism_reading, base_extractiveness, 1920, 0.62).

% Suppression requirement over time
narrative_ontology:measurement(plur_su_t1890, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1890, 0.61).
narrative_ontology:measurement(plur_su_t1896, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1896, 0.75).
narrative_ontology:measurement(plur_su_t1902, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1902, 0.82).
narrative_ontology:measurement(plur_su_t1908, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1908, 0.79).
narrative_ontology:measurement(plur_su_t1914, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1914, 0.76).
narrative_ontology:measurement(plur_su_t1920, plural_marriage_mandate__institutional_pragmatism_reading, suppression_requirement, 1920, 0.71).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(plural_marriage_mandate__institutional_pragmatism_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__endogenous_reinterpretation_reading).
narrative_ontology:affects_constraint(plural_marriage_mandate__institutional_pragmatism_reading, plural_marriage_mandate__exogenous_override_reading).

% DUAL FORMULATION NOTE:
% The plural_marriage_mandate kernel has been decomposed into three constraint stories, each instantiating a different structural reading of the 1890 Manifesto. The institutional-pragmatism reading (this story) models the Manifesto as strategic institutional adaptation entangling survival mechanisms with doctrinal legitimation. The endogenous-reinterpretation reading models it as genuine prophetic doctrinal update. The exogenous-override reading models it as coerced abandonment of a divine requirement. The three readings have different ε values, different beneficiary/victim structures, and different classifications (tangled_rope vs. rope vs. snare), reflecting their divergent structural analyses. They are linked via network.affects_constraints because they address the same kernel and their validity is mutually informative: empirical evidence about the authenticity of the covert continuations affects the plausibility of the pragmatism reading; evidence about the theological arguments affects the plausibility of the endogenous reading; evidence about federal coercive capacity affects the plausibility of the exogenous reading.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
