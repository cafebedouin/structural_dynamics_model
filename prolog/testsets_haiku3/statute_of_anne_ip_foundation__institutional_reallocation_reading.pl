% ============================================================================
% CONSTRAINT STORY: statute_of_anne_ip_foundation__institutional_reallocation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_statute_of_anne_ip_foundation__institutional_reallocation_reading, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: statute_of_anne_ip_foundation__institutional_reallocation_reading
 *   human_readable: Statute of Anne: Institutional Reallocation of Printing Rights
 *   domain: legal_history/intellectual_property/institutional_economics
 *
 * SUMMARY:
 *   The Statute of Anne (1710) is the first copyright statute in English law.
 *   This is ONE READING of the contested kernel
 *   'statute_of_anne_ip_foundation': the INSTITUTIONAL REALLOCATION reading.
 *   This reading frames the Statute as fundamentally a reallocation of
 *   institutional power and property rights — the Stationers' Company had
 *   held a monopoly on printing and publication decisions (granted by the
 *   Crown and maintained through guild discipline and guild-managed
 *   licensing). The Statute reallocated that monopoly power by granting
 *   authors (and their assignees) independent property rights to copy their
 *   own works, independent of Stationers' Company permission or membership.
 *   This transformed who occupied the institutional space: from a single
 *   guild controlling all publication decisions to a distributed system where
 *   individual authors (and publishers they assigned to) held property
 *   claims. The measured extractiveness (0.58 at interval end) reflects this
 *   reallocation still being asymmetric — authors gained a property right,
 *   but publishers (as assignees) and later corporate entities extracted
 *   value by accumulating and holding those rights; the bottleneck shifted
 *   from monopoly to consolidated copyright portfolios. This reading does NOT
 *   claim the Statute created a new concept of copyright (that is the
 *   conceptual_emergence_reading) or that institutional and conceptual change
 *   were inseparable (that is the entangled_event_reading). It claims the
 *   Statute's structural innovation was reallocating an existing
 *   institutional form — property rights and control — from one holder (the
 *   guild) to a new class (individual authors and their assignees). The
 *   claim/metric divergence is authored intentionally: the constraint is
 *   CLAIMED as tangled_rope (genuine coordination function — decentralizing
 *   publication decisions — AND asymmetric extraction — who holds rights and
 *   can assign them captures value), while the authored metrics show
 *   substantial extraction, active enforcement, and substantial resistance —
 *   exactly what a rope vs. snare divergence looks like from different seats.
 *
 * KEY AGENTS:
 *   - Stationers' Company: institutional monopoly holder, loses gatekeeping power and exclusive control over printing decisions after Statute reallocation
 *   - Authors (writing class): gain direct, heritable property rights independent of guild membership; new institutional standing as rights-holders
 *   - Publishers and competing booksellers: benefit from competitive entry — can negotiate author assignments directly instead of licensing through monopoly
 *   - Crown and Parliament: agenda-setter; reallocates rights by statute to break monopoly and incentivize authorship
 *   - Reading public: indirect beneficiary from reduced monopoly rents and broader publication decisions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58).
domain_priors:suppression_score(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.42).
domain_priors:theater_ratio(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, extractiveness, 0.58).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance, 0.72).

% --- Constraint claim ---
narrative_ontology:constraint_claim(statute_of_anne_ip_foundation__institutional_reallocation_reading, tangled_rope).
narrative_ontology:human_readable(statute_of_anne_ip_foundation__institutional_reallocation_reading, "Statute of Anne: Institutional Reallocation of Printing Rights").
narrative_ontology:topic_domain(statute_of_anne_ip_foundation__institutional_reallocation_reading, "legal_history/intellectual_property/institutional_economics").

domain_priors:requires_active_enforcement(statute_of_anne_ip_foundation__institutional_reallocation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'af21439f-59df-4692-a133-86822dfeaef5').
narrative_ontology:cs_kernel_codification('af21439f-59df-4692-a133-86822dfeaef5', fixed_text).
narrative_ontology:cs_authority_grounding('af21439f-59df-4692-a133-86822dfeaef5', lineage).
narrative_ontology:cs_interpretation_layer_present('af21439f-59df-4692-a133-86822dfeaef5').
narrative_ontology:cs_reading_relation('af21439f-59df-4692-a133-86822dfeaef5', statute_of_anne_ip_foundation__conceptual_emergence_reading, influences).
narrative_ontology:cs_reading_relation('af21439f-59df-4692-a133-86822dfeaef5', statute_of_anne_ip_foundation__entangled_event_reading, coexists_with).
narrative_ontology:cs_axiom('af21439f-59df-4692-a133-86822dfeaef5', foundational, property_rights_reallocation_primary).
narrative_ontology:cs_axiom_status(property_rights_reallocation_primary, holdable).
narrative_ontology:cs_axiom_grounding('af21439f-59df-4692-a133-86822dfeaef5', property_rights_reallocation_primary, conventional).
narrative_ontology:cs_axiom('af21439f-59df-4692-a133-86822dfeaef5', secondary, institutional_beneficiary_capture_contingent).
narrative_ontology:cs_axiom_status(institutional_beneficiary_capture_contingent, holdable).
narrative_ontology:cs_axiom_grounding('af21439f-59df-4692-a133-86822dfeaef5', institutional_beneficiary_capture_contingent, empirically_contingent).
narrative_ontology:cs_reference_frame('af21439f-59df-4692-a133-86822dfeaef5', monopoly_reallocation_frame).
narrative_ontology:cs_drift_state('af21439f-59df-4692-a133-86822dfeaef5', post_reallocation_consolidation, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('af21439f-59df-4692-a133-86822dfeaef5', '').
narrative_ontology:cs_kernel_id(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_and_publishers).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company_monopoly).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, competing_booksellers).
narrative_ontology:constraint_beneficiary(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public).
narrative_ontology:constraint_victim(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% A chartered guild that held monopoly printing rights and control over the book trade in England for over a century. The Statute directly reallocated their monopoly control by granting authors and their assignees independent copyright claims, reducing the Stationers' hold over what could be printed. They faced immediate loss of exclusive control over publication decisions and revenue streams from licensing.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, stationers_company, payer,
    institutional, generational, trapped, national).

% Gained a direct, transferable right to their written works independent of the Stationers' Company discretion. Authors could now authorize publication, assign rights to publishers or booksellers of their choosing, and collect licensing fees or royalties. This opened institutional space previously closed by the guild monopoly, enabling new market entrants and decentralizing publication decisions.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_and_publishers, beneficiary,
    powerful, generational, mobile, national).

% Entered the previously monopolized book trade by negotiating directly with authors for publication rights rather than acquiring them through Stationers' licensing. This shifted power from a single guild-controlled institution to distributed negotiation between authors and multiple publishers. They benefited from reduced barriers to entry.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, competing_booksellers, beneficiary,
    moderate, biographical, constrained, national).

% Gained access to a broader range of published works as publication decisions decentralized away from the Stationers' monopoly gatekeeping. More authors could be published; more publishers competed on selection and price. Benefited indirectly from reduced monopoly rents embedded in book prices.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, reading_public, beneficiary,
    powerless, biographical, constrained, national).

% Enacted the Statute, reallocating institutional control over printing rights from the Stationers' Company to a distributed system of author-based property claims. Justified the reallocation as encouraging learning and authorship by giving authors direct incentive to create, while limiting the perpetual monopoly that had characterized the guild system.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, crown_and_parliament, agenda_setter,
    institutional, generational, analytical, national).

% Printers and booksellers outside the Stationers' Company who had no legal standing under the old monopoly would have gained from the reallocation in theory, but faced practical barriers: they still needed to secure author agreements, build reputation, and navigate a now-complex rights landscape. Their structural position improved but remained constrained.
narrative_ontology:constraint_stakeholder(statute_of_anne_ip_foundation__institutional_reallocation_reading, excluded_unaffiliated_printers, excluded,
    powerless, biographical, trapped, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(statute_of_anne_ip_foundation__institutional_reallocation_reading, authors_and_publishers).
narrative_ontology:fixing_cost_class(statute_of_anne_ip_foundation__institutional_reallocation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Shifted publication decision-making and rights control from a single monopoly institution (Stationers' Company) to distributed negotiation between authors and multiple publishers, reducing bottlenecks and enabling broader dissemination of written works.
% TRANSFER_FUNCTION: Reallocated the property right to copy from the Stationers' Company (collective right held by membership) to individual authors (later assignable to publishers), moving control over publication decisions and revenue from institutional monopoly to distributed market actors.
% ABSENT_VOICES: Unaffiliated printers and booksellers outside the Company had no voice in the initial settlement; working authors without literary reputation or patron connections faced practical barriers to using new rights. The reading public's interests in access and affordability were secondary to author/publisher incentive structures.
% DISAPPEARANCE_RATIONALE: If the Statute had not reallocated rights from the Stationers' Company, printing and publishing would have remained a monopoly-controlled institution indefinitely—no independent author property claims, no competitive entry, no decentralized publication decisions. The entire subsequent book trade, authorship as a profession, and print capitalism depended on this institutional reallocation.
% FOUNDING_PROBLEM: The Stationers' Company monopoly created a bottleneck: the guild controlled what was published, authors had no direct claim to their works, unpopular ideas were suppressed through institutional gatekeeping, and excessive monopoly rents were embedded in book prices.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary parliamentary testimony and petitions from authors and independent booksellers complained of Stationers' monopoly abuses. Post-Statute analysis by economic historians documents reduced entry barriers, increased title output, and lower real book prices following the reallocation—corroborating that monopoly gatekeeping was a live problem the reallocation addressed.
narrative_ontology:disappearance_verdict(statute_of_anne_ip_foundation__institutional_reallocation_reading, world_rearranges).
narrative_ontology:founding_problem_status(statute_of_anne_ip_foundation__institutional_reallocation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(statute_of_anne_ip_foundation__institutional_reallocation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.58, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(statute_of_anne_ip_foundation__institutional_reallocation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(statute_of_anne_ip_foundation__institutional_reallocation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much value is asymmetrically captured by the constraint's operation. At t=0 (pre-Statute), the Stationers' monopoly extracted 0.72: absolute gatekeeping power, all publication decisions funneled through guild licensing, monopoly rents embedded in book prices. Post-Statute reallocation, extractiveness drops to 0.58 (t=50): the gatekeeping bottleneck is broken, publication decisions are decentralized, new publishers can enter via author rights assignments. But it doesn't drop further because (a) authors' rights are now assignable property, so publishers who acquire author rights can still extract monopoly-adjacent rents by controlling large portfolios, and (b) enforcement against unauthorized copying still requires suppressive action, now distributed across multiple rights-holders rather than a single monopoly. Suppression is high initially (0.68) because the monopoly required active enforcement through guild discipline and Crown-backed seizures. Post-Statute suppression drops (0.52 at t=50) because market mechanisms and distributed litigation replace monopoly enforcement. Theater is low-to-moderate (0.15–0.28 across interval) because the coordination function (decentralized publication decisions) is genuine and functionally operative; the reallocation genuinely does what it claims (incentivize authorship by giving authors property rights). The temporal series shows a clean reallocation path: steep initial drop in suppression and extraction as monopoly breaks, stabilization at a new equilibrium where extraction persists but via a different institutional mechanism (copyright property rather than guild gatekeeping). The coercion_grid captures level-differentiated dynamics: structural-level suppression drops sharply (monopoly enforcement → distributed property-rights litigation), but individual-level resistance actually INCREASES (more actors with standing to resist, more decentralized enforcement challenges to navigate). This matches the reallocation story: the Stationers as a unified institutional actor faced declining coercive capacity; competing authors, publishers, and readers faced MORE sites of potential enforcement (multiple copyright holders, distributed litigation) even though the overall extraction decreased.
 *
 * PERSPECTIVAL GAP:
 *   The Stationers' Company and the author/publisher class would experience this constraint radically differently. From the Stationers' seat: this is pure extraction (a targeted removal of their monopoly rights by parliamentary action), enforcement is against them (the Statute itself is the enforcement mechanism, removing their legal monopoly), and resistance is high but futile (they fought the Statute, lost, and faced 50+ years of market erosion). From the author/publisher seat: this is genuine coordination (decentralized publication decisions solve the bottleneck), the extraction is a residual (copyright-based rents that publishers capture), and resistance is about defending the new property rights against unauthorized copying. A payer computing the constraint would see it as snare-adjacent (we lost a monopoly, now face litigation risk, have no real alternative). A beneficiary computing it would see it as rope (coordination that happens to involve property rights, with some asymmetry but real net gain). The engine computes both seats' types from the structural data; the perspectival gap is the measurement signal.
 *
 * DIRECTIONALITY LOGIC:
 *   The Stationers' Company is the structural payer — they LOSE the reallocation. d for the Stationers: they are trapped (institutional actor with no alternative to the guild structure if it crumbles), they hold institutional power (but power the Statute specifically removes), they are at national scope where the reallocation happens. Their d sits near 1.0 (full target of the reallocation). Authors gain property rights (d moves toward 0.0, beneficiary end), but their exit options are still constrained (they need publishers to distribute, they depend on enforcement of their property rights against copying). Publishers and competitors gain from entry (d near beneficiary end), but they now depend on negotiating with fragmented author rights-holders rather than dealing with a single monopoly institution — their exit improves but remains constrained by the need to secure author agreements. The engine derives d automatically from beneficiary/victim + exit + power; the authored claim of tangled_rope (both coordination and asymmetric extraction present) means the Stationers compute as victim while authors/publishers compute as beneficiaries, but with different directionality profiles reflecting their different exit options and power standing.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Stationers' monopoly bottleneck on publication) is LIVE at t=0 and SUBSTANTIALLY REDUCED at t=50, but not eliminated. The Statute's mandate was to break monopoly gatekeeping and incentivize authorship; it succeeded at both (publication output rose, author class emerged, competitive entry occurred). But the constraint itself persists because copyright enforcement (prevention of unauthorized copying) remains necessary to implement the incentive function. This is NOT mandatrophy — the constraint still serves its founding purpose. The risk of mandatrophy emerges only much later (beyond t=50 in this reading's interval) if copyright terms are extended far beyond author lifespans and assignees begin capturing rents divorced from authorship incentives — at that point the founding problem (incentivizing authorship to break monopoly) becomes dead while copyright enforcement persists, triggering mandatrophy and potentially a reclassification from tangled_rope toward piton. At t=50, the reallocation is still functional; mandatrophy is a future risk, not a present condition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    reallocation_vs_creation,
    'Is the Statute''s institutional reallocation best characterized as reallocating pre-existing property rights from the Stationers'' Company, or creating a genuinely new conceptual/legal category (copyright as a limited monopoly distinct from perpetual guild monopoly)?',
    'Textual analysis of the Statute''s language; comparative law examining whether prior printing monopolies in other jurisdictions had similar structures; examination of contemporary parliamentary debate over whether IP was ''invented'' or ''transferred.''',
    'If reallocation of pre-existing rights, this reading stands as the primary structural account. If new creation, the conceptual_emergence_reading becomes the better frame and this reading becomes secondary — the reallocation becomes a means to an end (creating the new category) rather than the primary transformation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reallocation_vs_creation, conceptual, 'Whether the Statute reallocated existing rights or created new ones.').

omega_variable(
    institutional_vs_conceptual_primacy,
    'Does the institutional reallocation (who holds rights) drive the conceptual change (what copyright is), or does the conceptual innovation (limited monopoly for incentive) drive the institutional reallocation?',
    'Analysis of the causal chain: did Parliament first decide to redistribute rights to authors, then discover they needed a new conceptual framework to justify it? Or did they first theorize limited monopoly as an incentive mechanism, then implement it by reallocating from guild to authors?',
    'If institutional drives conceptual, this reading is primary and conceptual_emergence_reading is derivative. If conceptual drives institutional, the two readings are more tightly coupled and the entangled_event_reading''s account of simultaneity gains force.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(institutional_vs_conceptual_primacy, conceptual, 'The causal direction of institutional reallocation vs. conceptual innovation.').

omega_variable(
    stationers_displacement_mechanism,
    'Did the Statute immediately displace the Stationers'' Company''s institutional position (instantaneous reallocation), or did the Stationers gradually lose market position as authors increasingly assigned rights to competing publishers (reallocation-as-process)?',
    'Historical documentation of Stationers'' Company litigation, market share, and institutional functions in the decades following the Statute; analysis of how rapidly author-to-publisher-direct assignments became market norm.',
    'If instantaneous, the reallocation is a discrete institutional change event. If gradual, the constraint''s operation is one of market decentering rather than formal expropriation — a different type-path through enforcement dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(stationers_displacement_mechanism, empirical, 'The temporal profile of Stationers'' displacement.').

omega_variable(
    reallocation_beneficiary_intent,
    'Was the institutional reallocation primarily intended to benefit authors (new authors as a class gaining rights), or primarily intended to benefit publishers/booksellers (new competitive entry into a monopoly market), with author benefit as secondary or incidental?',
    'Parliamentary records, petitions, and testimony from the period; analysis of which actors lobbied for the Statute; outcomes for authors vs. publishers in the decades following.',
    'If authors were primary beneficiaries (intent and outcome), the constraint is author-centered reallocation. If publishers were primary beneficiaries (competitive entry), the constraint is more accurately a competitive-entry device that happened to route through author rights — a different structural reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(reallocation_beneficiary_intent, empirical, 'Primary beneficiary class in the reallocation.').

omega_variable(
    sibling_reading_frame_choice,
    'Is this reading''s emphasis on institutional reallocation as the primary transformation justified, or do the conceptual_emergence_reading (new legal category) or entangled_event_reading (simultaneous institutional+conceptual change) better capture the Statute''s actual operation?',
    'Corpus-level analysis: do historical outcomes (market share, litigation patterns, author outcomes) better fit institutional reallocation frames or conceptual innovation frames? Do institutional and conceptual metrics diverge or co-move?',
    'If institutional reallocation is the better frame, this reading''s type and beneficiary structure stand. If conceptual innovation better fits, the constraint reclassifies and the ε-referent shifts. If they are inseparable, the entangled_event_reading becomes primary.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(sibling_reading_frame_choice, conceptual, 'Whether institutional reallocation is the primary framing vs. sibling frames.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stat_tr_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 0, 0.25).
narrative_ontology:measurement_basis(stat_tr_t0, observed).
narrative_ontology:measurement(stat_tr_t5, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement_basis(stat_tr_t5, observed).
narrative_ontology:measurement(stat_tr_t15, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 15, 0.15).
narrative_ontology:measurement_basis(stat_tr_t15, observed).
narrative_ontology:measurement(stat_tr_t30, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement_basis(stat_tr_t30, observed).
narrative_ontology:measurement(stat_tr_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, theater_ratio, 50, 0.28).
narrative_ontology:measurement_basis(stat_tr_t50, projected).

% Extraction over time
narrative_ontology:measurement(stat_be_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 0, 0.72).
narrative_ontology:measurement_basis(stat_be_t0, observed).
narrative_ontology:measurement(stat_be_t5, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 5, 0.62).
narrative_ontology:measurement_basis(stat_be_t5, observed).
narrative_ontology:measurement(stat_be_t15, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 15, 0.56).
narrative_ontology:measurement_basis(stat_be_t15, observed).
narrative_ontology:measurement(stat_be_t30, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 30, 0.58).
narrative_ontology:measurement_basis(stat_be_t30, observed).
narrative_ontology:measurement(stat_be_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(stat_be_t50, projected).

% Suppression requirement over time
narrative_ontology:measurement(stat_su_t0, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 0, 0.68).
narrative_ontology:measurement_basis(stat_su_t0, observed).
narrative_ontology:measurement(stat_su_t5, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 5, 0.58).
narrative_ontology:measurement_basis(stat_su_t5, observed).
narrative_ontology:measurement(stat_su_t15, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 15, 0.48).
narrative_ontology:measurement_basis(stat_su_t15, observed).
narrative_ontology:measurement(stat_su_t30, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 30, 0.52).
narrative_ontology:measurement_basis(stat_su_t30, observed).
narrative_ontology:measurement(stat_su_t50, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression_requirement, 50, 0.52).
narrative_ontology:measurement_basis(stat_su_t50, projected).

% Leveled coercion grid (OQ-93): 32/32 authored points at t0=0, tn=50
narrative_ontology:measurement(stat_grid_01, statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse(class), 0, 0.85).
narrative_ontology:measurement_basis(stat_grid_01, observed).
narrative_ontology:measurement(stat_grid_02, statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse(class), 50, 0.62).
narrative_ontology:measurement_basis(stat_grid_02, projected).
narrative_ontology:measurement(stat_grid_03, statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse(individual), 0, 0.78).
narrative_ontology:measurement_basis(stat_grid_03, observed).
narrative_ontology:measurement(stat_grid_04, statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse(individual), 50, 0.55).
narrative_ontology:measurement_basis(stat_grid_04, projected).
narrative_ontology:measurement(stat_grid_05, statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse(organizational), 0, 0.88).
narrative_ontology:measurement_basis(stat_grid_05, observed).
narrative_ontology:measurement(stat_grid_06, statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse(organizational), 50, 0.58).
narrative_ontology:measurement_basis(stat_grid_06, projected).
narrative_ontology:measurement(stat_grid_07, statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse(structural), 0, 0.92).
narrative_ontology:measurement_basis(stat_grid_07, observed).
narrative_ontology:measurement(stat_grid_08, statute_of_anne_ip_foundation__institutional_reallocation_reading, accessibility_collapse(structural), 50, 0.68).
narrative_ontology:measurement_basis(stat_grid_08, projected).
narrative_ontology:measurement(stat_grid_09, statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance(class), 0, 0.65).
narrative_ontology:measurement_basis(stat_grid_09, observed).
narrative_ontology:measurement(stat_grid_10, statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance(class), 50, 0.78).
narrative_ontology:measurement_basis(stat_grid_10, projected).
narrative_ontology:measurement(stat_grid_11, statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance(individual), 0, 0.82).
narrative_ontology:measurement_basis(stat_grid_11, observed).
narrative_ontology:measurement(stat_grid_12, statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance(individual), 50, 0.85).
narrative_ontology:measurement_basis(stat_grid_12, projected).
narrative_ontology:measurement(stat_grid_13, statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance(organizational), 0, 0.72).
narrative_ontology:measurement_basis(stat_grid_13, observed).
narrative_ontology:measurement(stat_grid_14, statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance(organizational), 50, 0.68).
narrative_ontology:measurement_basis(stat_grid_14, projected).
narrative_ontology:measurement(stat_grid_15, statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance(structural), 0, 0.48).
narrative_ontology:measurement_basis(stat_grid_15, observed).
narrative_ontology:measurement(stat_grid_16, statute_of_anne_ip_foundation__institutional_reallocation_reading, resistance(structural), 50, 0.62).
narrative_ontology:measurement_basis(stat_grid_16, projected).
narrative_ontology:measurement(stat_grid_17, statute_of_anne_ip_foundation__institutional_reallocation_reading, stakes_inflation(class), 0, 0.72).
narrative_ontology:measurement_basis(stat_grid_17, observed).
narrative_ontology:measurement(stat_grid_18, statute_of_anne_ip_foundation__institutional_reallocation_reading, stakes_inflation(class), 50, 0.52).
narrative_ontology:measurement_basis(stat_grid_18, projected).
narrative_ontology:measurement(stat_grid_19, statute_of_anne_ip_foundation__institutional_reallocation_reading, stakes_inflation(individual), 0, 0.65).
narrative_ontology:measurement_basis(stat_grid_19, observed).
narrative_ontology:measurement(stat_grid_20, statute_of_anne_ip_foundation__institutional_reallocation_reading, stakes_inflation(individual), 50, 0.48).
narrative_ontology:measurement_basis(stat_grid_20, projected).
narrative_ontology:measurement(stat_grid_21, statute_of_anne_ip_foundation__institutional_reallocation_reading, stakes_inflation(organizational), 0, 0.78).
narrative_ontology:measurement_basis(stat_grid_21, observed).
narrative_ontology:measurement(stat_grid_22, statute_of_anne_ip_foundation__institutional_reallocation_reading, stakes_inflation(organizational), 50, 0.58).
narrative_ontology:measurement_basis(stat_grid_22, projected).
narrative_ontology:measurement(stat_grid_23, statute_of_anne_ip_foundation__institutional_reallocation_reading, stakes_inflation(structural), 0, 0.82).
narrative_ontology:measurement_basis(stat_grid_23, observed).
narrative_ontology:measurement(stat_grid_24, statute_of_anne_ip_foundation__institutional_reallocation_reading, stakes_inflation(structural), 50, 0.65).
narrative_ontology:measurement_basis(stat_grid_24, projected).
narrative_ontology:measurement(stat_grid_25, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression(class), 0, 0.62).
narrative_ontology:measurement_basis(stat_grid_25, observed).
narrative_ontology:measurement(stat_grid_26, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression(class), 50, 0.42).
narrative_ontology:measurement_basis(stat_grid_26, projected).
narrative_ontology:measurement(stat_grid_27, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression(individual), 0, 0.55).
narrative_ontology:measurement_basis(stat_grid_27, observed).
narrative_ontology:measurement(stat_grid_28, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression(individual), 50, 0.35).
narrative_ontology:measurement_basis(stat_grid_28, projected).
narrative_ontology:measurement(stat_grid_29, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression(organizational), 0, 0.68).
narrative_ontology:measurement_basis(stat_grid_29, observed).
narrative_ontology:measurement(stat_grid_30, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression(organizational), 50, 0.48).
narrative_ontology:measurement_basis(stat_grid_30, projected).
narrative_ontology:measurement(stat_grid_31, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression(structural), 0, 0.75).
narrative_ontology:measurement_basis(stat_grid_31, observed).
narrative_ontology:measurement(stat_grid_32, statute_of_anne_ip_foundation__institutional_reallocation_reading, suppression(structural), 50, 0.52).
narrative_ontology:measurement_basis(stat_grid_32, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(statute_of_anne_ip_foundation__institutional_reallocation_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(statute_of_anne_ip_foundation__institutional_reallocation_reading, 0.18).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__conceptual_emergence_reading).
narrative_ontology:affects_constraint(statute_of_anne_ip_foundation__institutional_reallocation_reading, statute_of_anne_ip_foundation__entangled_event_reading).

% DUAL FORMULATION NOTE:
% The Statute of Anne kernel decomposes into three structurally distinct readings: (1) institutional_reallocation_reading (THIS story) emphasizes the reallocation of property rights from guild to authors as the primary transformation; (2) conceptual_emergence_reading emphasizes that the Statute created a new legal category (limited monopoly for incentive) previously absent from English law; (3) entangled_event_reading claims institutional and conceptual change are inseparable — the Statute is a single event where both dimensions transformed simultaneously. Each reading produces a different constraint with different ε, different beneficiary/victim structure, and different classification. They are not measurements of the same constraint from different perspectives; they are three different constraints arising from three different readings of the same kernel. The ε-invariance principle required decomposition: a single constraint story could not assign both 'reallocation of pre-existing rights' and 'creation of a new conceptual category' without changing the referent (what is being extracted/coordinated). Each reading fixes its own referent: the reallocation reading's referent is 'the standing institutional arrangement before and after the Statute — the Stationers' monopoly vs. the distributed author-rights system.' The institutional reallocation reading emphasizes market transformation (breaking monopoly, enabling competitive entry) while the conceptual reading emphasizes legal innovation (copyright as a new category). They are linked via network.affects_constraints; downstream consumers model the kernel contest by reading all three stories and computing which reading's framing best fits the constraint-event data.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
