% ============================================================================
% CONSTRAINT STORY: sovereign_legitimacy__monarchical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_sovereign_legitimacy__monarchical_reading, []).

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
 *   constraint_id: sovereign_legitimacy__monarchical_reading
 *   human_readable: Hereditary Sovereign Legitimacy (Monarchical Reading — Divine Right and Bloodline)
 *   domain: political philosophy/constitutional theory/legitimacy studies
 *
 * SUMMARY:
 *   This story instantiates the monarchical reading of the
 *   sovereign_legitimacy kernel: legitimate authority descends from the
 *   sovereign through inherited right, validated by divine sanction
 *   (anointing and coronation oath), tradition, and unbroken bloodline;
 *   subjects owe obedience as a duty owed to God's ordinance and hold no
 *   share in authorizing rule. The measurement window tracks the English arc
 *   of the strong reading: the Stuart assertion of divine right from 1603,
 *   the enforcement ratchet of the 1630s personal rule, the civil-war
 *   collapse and republican interregnum of 1649-60, the restoration's
 *   hardened enforcement (Clarendon Code, conformity acts, oath rolls), the
 *   Exclusion Crisis, and the reading's restored peak in 1685 on the eve of
 *   the parliamentary settlement that would override its core premise. The
 *   interval closes at 1685 by design — the vantage is the reading as a
 *   standing, enforced arrangement at full strength; the 1689-1701 settlement
 *   is recorded in the drift state and omegas rather than in the measurement
 *   grid. KEY AGENTS (by structural relationship): hereditary_ruling_house
 *   (agenda-setter and primary beneficiary), landed_aristocracy (secondary
 *   beneficiary with payer exposure), established_church (beneficiary holding
 *   the legitimation monopoly), common_subjects (primary target),
 *   republican_and_contractualist_dissenters (suppressed target with excluded
 *   seat), rival_dynastic_claimants (excluded), political_philosophers
 *   (analytical observer).
 *
 * KEY AGENTS:
 *   - hereditary_ruling_house: agenda-setter and primary beneficiary (institutional / identity_locked) — holds the throne by descent, collects taxation and office-granting power, enforces the succession rule
 *   - landed_aristocracy: secondary beneficiary with payer exposure (organized / constrained) — collects peerage, office, and patronage through the descending hierarchy while bearing subordination to the crown
 *   - established_church: beneficiary (institutional / identity_locked) — holds the anointing and legitimation monopoly; its apostolic-succession governance mirrors the royal descent claim it certifies
 *   - common_subjects: primary target (powerless / trapped) — bear taxation, tithes, and military service with no share in authorizing who rules
 *   - republican_and_contractualist_dissenters: suppressed target with excluded seat (moderate / constrained) — advance alternative legitimacy claims and bear prosecution, censorship, and exile for it
 *   - rival_dynastic_claimants: excluded (powerful / trapped) — hold bloodline claims the succession rule cannot absorb; each crisis draws them toward rebellion
 *   - political_philosophers: analytical observer (analytical / analytical) — Filmer, Hobbes, Locke, and Sidney map the structure from outside every seat
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, 0.72).
domain_priors:suppression_score(sovereign_legitimacy__monarchical_reading, 0.8).
domain_priors:theater_ratio(sovereign_legitimacy__monarchical_reading, 0.33).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, extractiveness, 0.72).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, theater_ratio, 0.33).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(sovereign_legitimacy__monarchical_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(sovereign_legitimacy__monarchical_reading, tangled_rope).
narrative_ontology:human_readable(sovereign_legitimacy__monarchical_reading, "Hereditary Sovereign Legitimacy (Monarchical Reading — Divine Right and Bloodline)").
narrative_ontology:topic_domain(sovereign_legitimacy__monarchical_reading, "political philosophy/constitutional theory/legitimacy studies").

domain_priors:requires_active_enforcement(sovereign_legitimacy__monarchical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(sovereign_legitimacy__monarchical_reading, '5b81f5ac-0072-4d7e-9284-eb32695c3c36').
narrative_ontology:cs_kernel_codification('5b81f5ac-0072-4d7e-9284-eb32695c3c36', fixed_text).
narrative_ontology:cs_authority_grounding('5b81f5ac-0072-4d7e-9284-eb32695c3c36', lineage).
narrative_ontology:cs_interpretation_layer_present('5b81f5ac-0072-4d7e-9284-eb32695c3c36').
narrative_ontology:cs_reading_relation('5b81f5ac-0072-4d7e-9284-eb32695c3c36', sovereign_legitimacy__republican_reading, forecloses).
narrative_ontology:cs_reading_relation('5b81f5ac-0072-4d7e-9284-eb32695c3c36', sovereign_legitimacy__constitutional_hybrid_reading, forecloses).
narrative_ontology:cs_axiom('5b81f5ac-0072-4d7e-9284-eb32695c3c36', foundational, legitimate_authority_descends_divinely).
narrative_ontology:cs_axiom_status(legitimate_authority_descends_divinely, holdable).
narrative_ontology:cs_axiom_grounding('5b81f5ac-0072-4d7e-9284-eb32695c3c36', legitimate_authority_descends_divinely, theological).
narrative_ontology:cs_axiom('5b81f5ac-0072-4d7e-9284-eb32695c3c36', secondary, bloodline_continuity_validates_succession).
narrative_ontology:cs_axiom_status(bloodline_continuity_validates_succession, holdable).
narrative_ontology:cs_axiom_grounding('5b81f5ac-0072-4d7e-9284-eb32695c3c36', bloodline_continuity_validates_succession, conventional).
narrative_ontology:cs_reference_frame('5b81f5ac-0072-4d7e-9284-eb32695c3c36', divinely_ordained_hereditary_sovereignty).
narrative_ontology:cs_drift_state('5b81f5ac-0072-4d7e-9284-eb32695c3c36', post_glorious_revolution_settlement, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('5b81f5ac-0072-4d7e-9284-eb32695c3c36', '').
narrative_ontology:cs_kernel_id(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, hereditary_ruling_house).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, landed_aristocracy).
narrative_ontology:constraint_beneficiary(sovereign_legitimacy__monarchical_reading, established_church).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, common_subjects).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, republican_and_contractualist_dissenters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(sovereign_legitimacy__monarchical_reading, landed_aristocracy).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, divine_right_doctrine).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, hereditary_succession_principle).
narrative_ontology:constraint_vindicates(sovereign_legitimacy__monarchical_reading, patriarchal_authority_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Holds the crown by birth and passes it by bloodline. Sets succession rules, summons and dissolves parliaments, commands the armed forces, appoints judges and ministers, and collects taxation on its own prerogative where parliament resists. Its claim to rule rests on anointing at coronation and unbroken descent; abdication or sharing the claim would dissolve the basis of its own authority, so the house cannot step outside the arrangement it administers. Dynastic marriages tie it to royal houses across Europe.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, hereditary_ruling_house, agenda_setter,
    institutional, generational, identity_locked, continental).

% Holds peerages, estates, and local offices that flow from the crown's patronage; sits in the House of Lords and administers the countryside as justices of the peace. Its rank and legal privileges exist only inside the descending hierarchy — a title comes with the bloodline order or not at all. At the same time it answers to the crown: estates can be taxed, leaders attainted or executed, and prerogatives curtailed, so it both collects from the order and bears its subordination.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, landed_aristocracy, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, landed_aristocracy, payer).

% Supplies the divine-sanction leg of the claim: crowns and anoints the sovereign, preaches obedience to ordained powers, staffs the courts of High Commission, and holds the legitimation monopoly. Its own governance is built on apostolic succession — a descent structure mirroring the royal one — so the doctrine it certifies is also the doctrine that certifies its own office. Dissenting congregations are fined, imprisoned, or dispersed under conformity statutes it helps administer.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, established_church, beneficiary,
    institutional, generational, identity_locked, national).

% Owe allegiance, taxes, tithes, and military service; hold no share in choosing or authorizing who rules them. Protection, justice, and parish order flow back down, but the terms are set entirely above them. Oath rolls, parish officers, and treason law reach into every locality; leaving the realm means forfeiting livelihood, family, and legal standing, so exit is theoretical for nearly all.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, common_subjects, payer,
    powerless, biographical, trapped, national).

% Publish and organize around the claim that authority originates in the people and may be withdrawn — through pamphlets, petitions, gathered congregations, and at moments the army's councils. They are excluded from any seat in the settlement of legitimacy: their books are burned or licensed, their leaders prosecuted for treason or sedition, and their meetings suppressed under conformity acts. Some flee to Holland or the colonies; most remain under surveillance.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, republican_and_contractualist_dissenters, payer,
    moderate, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(sovereign_legitimacy__monarchical_reading, republican_and_contractualist_dissenters, excluded).

% Hold competing bloodline claims the succession rule cannot absorb — legitimated or illegitimate branches, deposed lines, and foreign-backed pretenders. The validation mechanism admits no testimony from them: their claim is their identity, so they cannot accept the rule that excludes them, and each succession crisis draws them toward invasion or rebellion with foreign patrons behind them.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, rival_dynastic_claimants, excluded,
    powerful, generational, trapped, continental).

% Map the structure from outside every seat: Filmer grounds descent in paternal dominion extended from Adam; Hobbes builds obedience on fear and covenant while denying hereditary right's distinctiveness; Locke and Sidney dismantle the descent premise and re-source authority in consent. None commands armies or collects rents; their contest is over which account of legitimacy the next settlement will cite.
narrative_ontology:constraint_stakeholder(sovereign_legitimacy__monarchical_reading, political_philosophers, observer,
    analytical, civilizational, analytical, continental).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(sovereign_legitimacy__monarchical_reading, hereditary_ruling_house).
narrative_ontology:fixing_cost_class(sovereign_legitimacy__monarchical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Settles the succession problem and provides a single focal point of sovereignty: hereditary succession removes the contest over 'who commands' that would otherwise reopen at every transfer of supreme authority, and the bloodline rule plus ritual validation gives the polity continuity, a permanent symbol of unity, and a settled answer that survives its holder's death without a new founding act.
% TRANSFER_FUNCTION: Moves obedience, taxation, tithes, military service, and legitimacy-recognition upward from subjects to the sovereign and the hereditary hierarchy; moves protection, justice, office, patronage, and legitimacy-sanction downward from the crown through the aristocratic and clerical hierarchy. Subjects receive no share in authorizing the arrangement itself.
% ABSENT_VOICES: Republican and contractualist theorists, gathered congregations, and common subjects have no seat: the validation mechanism (bloodline plus anointing) admits no testimony from the governed, and dissenting voices are criminalized as treason or sedition or excluded from any franchise. They would contest the descent premise itself — that authority ever descended, rather than ascended.
% DISAPPEARANCE_RATIONALE: Courts, offices, oaths, the succession itself, and the church's jurisdiction are all organized around the descent claim; overnight disappearance forces the polity to re-found authority on some other source — as the 1649 Commonwealth and the 1689 settlement each demonstrated, at the cost of civil war and constitutional rupture — rather than leaving the world roughly as it was.
% FOUNDING_PROBLEM: Every transfer of supreme authority reopens the question of who may command, and unresolved succession invites civil war among claimants; the arrangement was built to settle succession by rule (bloodline primacy) and to anchor obedience in a sanction above political contest (divine ordination), so that authority survives its holder's death.
% FOUNDING_PROBLEM_CORROBORATION: The problem is corroborated from outside the benefiting parties: the succession-crisis record itself (the Wars of the Roses; the 1640-49 collapse when enforcement broke) attests that unsettled succession meant civil war, and theorists who rejected the bloodline solution — Sidney, Locke, and Hobbes, no friend of hereditary claim — all concede the succession-ordering problem is real. No corroborating source outside the beneficiary set attests the divine-sanction mechanism specifically: that element rests on the established church's testimony, and the church is itself a beneficiary of the arrangement it certifies.
narrative_ontology:disappearance_verdict(sovereign_legitimacy__monarchical_reading, world_rearranges).
narrative_ontology:founding_problem_status(sovereign_legitimacy__monarchical_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(sovereign_legitimacy__monarchical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth3', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(sovereign_legitimacy__monarchical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(sovereign_legitimacy__monarchical_reading, 0.72, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sovereign_legitimacy__monarchical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(sovereign_legitimacy__monarchical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(sovereign_legitimacy__monarchical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction is high (0.72 at interval end) because the arrangement transfers the whole authority package — taxation, office, military command, and the monopoly on authorizing rule — to a hereditary house and its hierarchy, with subjects' obligations set entirely above them; protection and order flow back, but the terms are not negotiable by those bound by them. Suppression (0.80) is the load-bearing wall: treason law, licensing and censorship, oath rolls, conformity statutes, and the prerogative courts criminalize alternative legitimacy claims rather than outcompeting them. Theater (0.33) is real but subordinate: coronation and anointing ARE the validation mechanism, so the ritual is functional, yet a growing share of ceremony is detached from enforcement reality. Accessibility collapse (0.45) is moderate because alternatives demonstrably exist and function — the Dutch Republic, Venetian election, the 1649-60 Commonwealth — so the reading must actively suppress rivals rather than merely precede them. Resistance (0.70) reflects sustained organized challenge, including the latent coalition power of subjects and dissenters, which briefly broke the arrangement in 1642-49 and re-formed around the Exclusion Crisis. The claimed type (tangled_rope) is authored from structure — a genuine succession-coordination function joined to asymmetric extraction through the same descent hierarchy, held by active enforcement — independently of these metric values. The measurement series share one time grid (1603, 1625, 1640, 1649, 1660, 1670, 1681, 1685) so every tracked metric is authored at every examined point; the series trace one full crisis-recovery cycle: enforcement build-up (1603-40), collapse with the monarchy (1649, when theater persists without function and enforcement machinery is abolished), restoration overshoot (1660, enforcement rebuilt harder than before the war — a ratchet, not a return), partial relaxation (1670), and re-tightening through the Exclusion Crisis to the 1685 peak, where base_properties are measured.
 *
 * PERSPECTIVAL GAP:
 *   The seats compute differently. From the throne, the arrangement is not a burden on anyone: it is the frame of order itself, with duties running reciprocally (protection down, obedience up), and the descent claim needs no defense because it is the premise of every office, including the judges who would hear a challenge. From the subject seat, the same structure is taxation and service without any share in authorization, enforced by treason statute. The aristocracy sits astride the divide — collectors of the hierarchy's offices and subordinates of the crown at once — so its seat should compute mixed rather than clean. The church's seat fuses doctrine and interest: the divine-sanction premise it certifies is the same apostolic-descent structure that certifies its own office, so its perception of the arrangement as sacred is also a perception of itself as sacred. The dissenters' seat sees only the enforcement face, having no access to the coordination benefits the insiders cite.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality for the hereditary_ruling_house (collects the sovereignty package directly — the seat the gains demonstrably accrue to), the established_church (collects the legitimation monopoly), and the landed_aristocracy (collects office and patronage, though its payer secondary-role damps the subsidy toward the middle). Victim declarations drive high directionality for common_subjects (bear taxation, tithes, conscription, and total exclusion from authorization; trapped exit pushes them toward the full-target end) and republican_and_contractualist_dissenters (bear prosecution and censorship specifically for contesting the claim). National spatial scope amplifies effective extraction modestly for the target seats — grievance verification across a realm is slow and the enforcement apparatus is everywhere. Suppression is authored as a raw structural property and is not scaled by power or scope: the 0.80 is the coercive machinery itself, the same in kind for every seat it touches.
 *
 * MANDATROPHY ANALYSIS:
 *   The tangled_rope classification prevents mislabeling in both directions. Reading the arrangement as pure enforcement of a monopoly (the dissenters' account) would erase the genuine coordination function: the bloodline rule really did settle transfers of supreme authority for centuries, and its breakdown shows what it coordinated — the 1649-60 interregnum never solved succession (the Protectorate faced the same crisis at Cromwell's death), and the contests of 1685-1701 were fought precisely over the rule's content. Reading it as the crown's own frame — a natural, divinely ordained order with reciprocal duties and no asymmetry anywhere — would erase the structure the victim declarations record: obligations subjects had no share in setting, alternatives criminalized rather than outcompeted, gains accruing to a named seat. The founding problem (settling succession without reopening civil war at every death) is live, so no mandatrophy resolution is declared: the arrangement has not outlived its function, though the post-settlement persistence omega tracks whether defeat will eventually convert it into maintained performance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_contest,
    'This constraint is the monarchical_reading of the sovereign_legitimacy kernel. Do the republican_reading or constitutional_hybrid_reading instantiate different constraints with different beneficiary/victim structures, and does the kernel''s text underdetermine which reading is correct?',
    'Track which validation mechanism commands allegiance after each succession crisis: if allegiance follows bloodline and anointing, this reading holds; if it follows electoral or parliamentary consent, a sibling reading has displaced this one as the operative arrangement.',
    'The siblings are separate constraints with inverted or mixed beneficiary/victim structures; resolving the contest in favor of a sibling does not change this story''s classification — it retires this story as the operative arrangement and activates the sibling''s file.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest, conceptual, 'Committer structure: one reading of a contested kernel; siblings would restructure beneficiaries and victims entirely.').

omega_variable(
    divine_sanction_unfalsifiability,
    'Is the divine-sanction validation mechanism a genuine legitimacy ground or an unfalsifiable post-hoc sanctification of possession?',
    'Observe the mechanism''s response to failure cases: when an anointed, legitimate-bloodline sovereign is deposed or defeated, do adherents treat the event as refuting the mechanism or as persecution of the true order (the non-juring and Jacobite response to 1688 suggests the latter)?',
    'If the mechanism is unfalsifiable by design, argument cannot dislodge the reading and only suppression or exhaustion can — persistence then leans harder on enforcement, pushing computed classifications toward the extractive end; if falsifiable, the reading competes as an ordinary legitimacy claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_sanction_unfalsifiability, conceptual, 'Whether the reading''s validation mechanism can lose on the evidence.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is subject obedience to the descent claim structural (treason law, oath rolls, church courts, censorship) or internalized (genuine belief in divine ordination and paternal hierarchy)?',
    'Post-suppression trajectory: where enforcement lapsed (the 1649-60 interregnum, the 1688 army defection), did allegiance persist to the bloodline or transfer to the new arrangement within a generation?',
    'If largely internalized, effective suppression exceeds the structural measure and the reading survives enforcement collapse (Jacobitism after 1688 supports this); if structural, removing enforcement dissolves the reading quickly and the measured suppression is the whole story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural versus internalized obedience mechanism.').

omega_variable(
    succession_rule_efficiency,
    'Does the bloodline validation mechanism reduce succession violence or merely relocate it — settling who rules while raising the stakes of whose line?',
    'Compare frequency and severity of succession conflicts across hereditary and elective or contractual succession regimes in comparable polities over the same period.',
    'If the mechanism relocates rather than reduces contest (the 1685 Monmouth rising and the coming 1688 crisis suggest relocation), the coordination component is weaker than the arrangement claims and the structure sits closer to pure enforcement of a monopoly than to balanced coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(succession_rule_efficiency, empirical, 'Whether the founding coordination function delivers what it claims.').

omega_variable(
    patriarchal_naturalness_claim,
    'Is hereditary descent a natural feature of political authority (Filmer: all authority is paternal dominion extended from Adam, hence no constructed arrangement at all) or a built order benefiting identifiable hereditary and clerical interests?',
    'Test the natural-law claim against polities where authority demonstrably ascends (Venice, the Dutch Republic, the 1649-60 Commonwealth) without collapse into disorder: functioning alternatives show the descent structure is chosen, not compelled.',
    'If the natural-law reading held, the arrangement would present as a fixed feature with no beneficiaries to name; the declared beneficiaries and the functioning republican alternatives both point to construction — resolving the omega toward construction confirms the declared structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(patriarchal_naturalness_claim, conceptual, 'Natural law versus constructed order: the Filmerian claim against the observable alternatives.').

omega_variable(
    post_settlement_persistence,
    'After the parliamentary settlement overrides the reading''s core premise (1689-1701), does the reading persist with live adherents willing to enforce it, or decay into ceremonial performance?',
    'Track the Jacobite risings (1715, 1745), the non-juring schism, and the coronation ritual''s functional content: enforcement-capable adherence keeps the reading live; ritual-only adherence makes it vestigial.',
    'Live enforcement-capable adherence keeps this story''s classification operative beyond the interval; ritual-only persistence would date the reading''s effective end near the settlement and reclassify its residue as maintained performance.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(post_settlement_persistence, empirical, 'Whether the defeated reading remains a live arrangement or becomes vestigial.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(sovereign_legitimacy__monarchical_reading, 1603, 1685).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(sove_tr_t1603, sovereign_legitimacy__monarchical_reading, theater_ratio, 1603, 0.18).
narrative_ontology:measurement_basis(sove_tr_t1603, observed).
narrative_ontology:measurement(sove_tr_t1625, sovereign_legitimacy__monarchical_reading, theater_ratio, 1625, 0.22).
narrative_ontology:measurement_basis(sove_tr_t1625, observed).
narrative_ontology:measurement(sove_tr_t1640, sovereign_legitimacy__monarchical_reading, theater_ratio, 1640, 0.27).
narrative_ontology:measurement_basis(sove_tr_t1640, observed).
narrative_ontology:measurement(sove_tr_t1649, sovereign_legitimacy__monarchical_reading, theater_ratio, 1649, 0.82).
narrative_ontology:measurement_basis(sove_tr_t1649, observed).
narrative_ontology:measurement(sove_tr_t1660, sovereign_legitimacy__monarchical_reading, theater_ratio, 1660, 0.28).
narrative_ontology:measurement_basis(sove_tr_t1660, observed).
narrative_ontology:measurement(sove_tr_t1670, sovereign_legitimacy__monarchical_reading, theater_ratio, 1670, 0.3).
narrative_ontology:measurement_basis(sove_tr_t1670, observed).
narrative_ontology:measurement(sove_tr_t1681, sovereign_legitimacy__monarchical_reading, theater_ratio, 1681, 0.31).
narrative_ontology:measurement_basis(sove_tr_t1681, observed).
narrative_ontology:measurement(sove_tr_t1685, sovereign_legitimacy__monarchical_reading, theater_ratio, 1685, 0.33).
narrative_ontology:measurement_basis(sove_tr_t1685, observed).

% Extraction over time
narrative_ontology:measurement(sove_be_t1603, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1603, 0.6).
narrative_ontology:measurement_basis(sove_be_t1603, observed).
narrative_ontology:measurement(sove_be_t1625, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1625, 0.66).
narrative_ontology:measurement_basis(sove_be_t1625, observed).
narrative_ontology:measurement(sove_be_t1640, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1640, 0.74).
narrative_ontology:measurement_basis(sove_be_t1640, observed).
narrative_ontology:measurement(sove_be_t1649, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1649, 0.12).
narrative_ontology:measurement_basis(sove_be_t1649, observed).
narrative_ontology:measurement(sove_be_t1660, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1660, 0.64).
narrative_ontology:measurement_basis(sove_be_t1660, observed).
narrative_ontology:measurement(sove_be_t1670, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1670, 0.62).
narrative_ontology:measurement_basis(sove_be_t1670, observed).
narrative_ontology:measurement(sove_be_t1681, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1681, 0.66).
narrative_ontology:measurement_basis(sove_be_t1681, observed).
narrative_ontology:measurement(sove_be_t1685, sovereign_legitimacy__monarchical_reading, base_extractiveness, 1685, 0.72).
narrative_ontology:measurement_basis(sove_be_t1685, observed).

% Suppression requirement over time
narrative_ontology:measurement(sove_su_t1603, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1603, 0.55).
narrative_ontology:measurement_basis(sove_su_t1603, observed).
narrative_ontology:measurement(sove_su_t1625, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1625, 0.66).
narrative_ontology:measurement_basis(sove_su_t1625, observed).
narrative_ontology:measurement(sove_su_t1640, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1640, 0.76).
narrative_ontology:measurement_basis(sove_su_t1640, observed).
narrative_ontology:measurement(sove_su_t1649, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1649, 0.08).
narrative_ontology:measurement_basis(sove_su_t1649, observed).
narrative_ontology:measurement(sove_su_t1660, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1660, 0.78).
narrative_ontology:measurement_basis(sove_su_t1660, observed).
narrative_ontology:measurement(sove_su_t1670, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1670, 0.7).
narrative_ontology:measurement_basis(sove_su_t1670, observed).
narrative_ontology:measurement(sove_su_t1681, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1681, 0.74).
narrative_ontology:measurement_basis(sove_su_t1681, observed).
narrative_ontology:measurement(sove_su_t1685, sovereign_legitimacy__monarchical_reading, suppression_requirement, 1685, 0.8).
narrative_ontology:measurement_basis(sove_su_t1685, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(sovereign_legitimacy__monarchical_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__republican_reading).
narrative_ontology:affects_constraint(sovereign_legitimacy__monarchical_reading, sovereign_legitimacy__constitutional_hybrid_reading).

% DUAL FORMULATION NOTE:
% The colloquial label 'sovereign legitimacy' decomposes into three structurally distinct constraints — the monarchical, republican, and constitutional-hybrid readings of one kernel. Each reading has its own epsilon, its own beneficiary/victim structure, and its own validation mechanism: this story authors the monarchical reading (descent, divine sanction, bloodline; hereditary class benefits; subjects excluded); the republican reading inverts the structure (consent ascends; office temporary); the hybrid splits authority (inherited ceremony, delegated power). They are linked as a constraint family: whichever reading is operative determines which of the three files describes the standing arrangement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
