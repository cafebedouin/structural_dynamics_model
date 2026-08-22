% ============================================================================
% CONSTRAINT STORY: marriage_authority_kernel__christian_canonical_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_marriage_authority_kernel__christian_canonical_reading, []).

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
 *   constraint_id: marriage_authority_kernel__christian_canonical_reading
 *   human_readable: Christian Canonical Authority over Marriage and Family (Indian Christian Marriage Act 1872)
 *   domain: comparative_law/constitutional_pluralism/religious_governance
 *
 * SUMMARY:
 *   This story instantiates ONE reading of the marriage-authority kernel: the
 *   claim that marriage and family law authority for Indian Christians
 *   derives from Christian canonical law as codified in the Indian Christian
 *   Marriage Act 1872, administered through church structures and the
 *   associated divorce legislation. The standing arrangement under contest —
 *   the epsilon referent — is that canonical-derived governance as it
 *   operates today: state-recognized solemnization and registration,
 *   fault-based dissolution with a mutual-consent route added in 2001, church
 *   matrimonial tribunals handling nullity, and congregational discipline
 *   backed by the legal frame. Stated assumptions: (i) the 1872 Act is read
 *   together with its dissolution apparatus (the Indian Divorce Act 1869 as
 *   amended in 2001) as one standing arrangement, because the kernel claim
 *   concerns the SOURCE of authority rather than any single statute, and
 *   splitting solemnization from dissolution would manufacture two epsilon
 *   values for what the reading holds as one delegation of authority; (ii)
 *   epsilon is authored from this reading's own lights — the churches and
 *   faithful who hold the reading assess the arrangement as legitimate
 *   coordination carrying acknowledged burdens — yielding a moderate value
 *   rather than the higher value a reformist seat would author over the
 *   identical referent; sibling readings are separate files with their own
 *   epsilon and victim sets and are not averaged here. Claim and metrics are
 *   independent: claimed_type records my structural judgment (a real
 *   recognition-and-succession coordination function combined with
 *   asymmetric, actively enforced burden = tangled_rope); the metrics record
 *   the arrangement's observed operation. No reconciliation between claim and
 *   metrics is performed. KEY AGENTS (by structural relationship): -
 *   church_hierarchies: Agenda-setting institution
 *   (institutional/identity_locked) — defines valid marriage, staffs
 *   tribunals, defends the reading - indian_state_legislature_judiciary:
 *   Co-agenda-setter with secondary collection (institutional/arbitrage) —
 *   maintains the statute, absorbs reform politics -
 *   clergy_tribunal_officials: Beneficiary (organized/constrained) — collect
 *   standing and roles from the tribunal system -
 *   conservative_community_elders: Beneficiary (organized/constrained) —
 *   enforce congregational expectations backed by the legal frame -
 *   women_in_failed_marriages: Primary bearer of costs
 *   (powerless/constrained) — restricted, historically gendered dissolution
 *   access - spouses_without_fault_grounds: Secondary bearer of costs
 *   (moderate/constrained) — no ground, no exit -
 *   children_of_stalled_marriages: Absent voice (powerless/trapped) — live
 *   inside undissolvable marriages, never heard -
 *   comparative_family_law_scholars: Analytical observer
 *   (analytical/analytical) — sees the full structure from outside
 *
 * KEY AGENTS:
 *   - church_hierarchies: Agenda-setting institution (institutional/identity_locked) — administers tribunals, defines canonical discipline, cannot exit its own jurisdiction without dissolving its self-understanding
 *   - indian_state_legislature_judiciary: Co-agenda-setter with secondary collection (institutional/arbitrage) — maintains the 1872 settlement, delegates family governance, absorbs the political cost of reform
 *   - clergy_tribunal_officials: Beneficiary (organized/constrained) — staffing and standing depend on the tribunal system continuing
 *   - conservative_community_elders: Beneficiary (organized/constrained) — congregational teaching backed by the legal frame
 *   - women_in_failed_marriages: Primary bearer of costs (powerless/constrained) — restricted and historically gendered dissolution access; coalition capacity through women's fellowships and reform networks
 *   - spouses_without_fault_grounds: Secondary bearer of costs (moderate/constrained) — matched to no statutory ground, stranded unless both spouses consent
 *   - children_of_stalled_marriages: Absent voice (powerless/trapped) — absorb prolonged parental conflict, heard in no proceeding
 *   - comparative_family_law_scholars: Analytical observer (analytical/analytical) — publish and testify from outside the dispute
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, 0.48).
domain_priors:suppression_score(marriage_authority_kernel__christian_canonical_reading, 0.55).
domain_priors:theater_ratio(marriage_authority_kernel__christian_canonical_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, extractiveness, 0.48).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(marriage_authority_kernel__christian_canonical_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(marriage_authority_kernel__christian_canonical_reading, tangled_rope).
narrative_ontology:human_readable(marriage_authority_kernel__christian_canonical_reading, "Christian Canonical Authority over Marriage and Family (Indian Christian Marriage Act 1872)").
narrative_ontology:topic_domain(marriage_authority_kernel__christian_canonical_reading, "comparative_law/constitutional_pluralism/religious_governance").

domain_priors:requires_active_enforcement(marriage_authority_kernel__christian_canonical_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(marriage_authority_kernel__christian_canonical_reading, '5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a').
narrative_ontology:cs_kernel_codification('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', fixed_text).
narrative_ontology:cs_authority_grounding('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', lineage).
narrative_ontology:cs_interpretation_layer_present('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a').
narrative_ontology:cs_reading_relation('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', marriage_authority_kernel__hindu_codified_reading, coexists_with).
narrative_ontology:cs_reading_relation('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', marriage_authority_kernel__muslim_shariat_reading, coexists_with).
narrative_ontology:cs_reading_relation('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', marriage_authority_kernel__parsi_communal_reading, coexists_with).
narrative_ontology:cs_reading_relation('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', marriage_authority_kernel__secular_civil_reading, influences).
narrative_ontology:cs_axiom('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', foundational, marriage_sacrament_not_revocable_contract).
narrative_ontology:cs_axiom_status(marriage_sacrament_not_revocable_contract, holdable).
narrative_ontology:cs_axiom_grounding('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', marriage_sacrament_not_revocable_contract, theological).
narrative_ontology:cs_axiom('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', foundational, ecclesiastical_jurisdiction_over_family_life).
narrative_ontology:cs_axiom_status(ecclesiastical_jurisdiction_over_family_life, holdable).
narrative_ontology:cs_axiom_grounding('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', ecclesiastical_jurisdiction_over_family_life, conventional).
narrative_ontology:cs_reference_frame('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', canonical_sacramental_marriage_order).
narrative_ontology:cs_drift_state('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', contemporary_ucc_debate_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('5339fba3-5c85-4a8c-b78b-dbd2cd9d8e6a', '').
narrative_ontology:cs_kernel_id(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, church_hierarchies).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, clergy_tribunal_officials).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, conservative_community_elders).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, women_in_failed_marriages).
narrative_ontology:constraint_victim(marriage_authority_kernel__christian_canonical_reading, spouses_without_fault_grounds).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(marriage_authority_kernel__christian_canonical_reading, indian_state_legislature_judiciary).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, sacramental_indissolubility_doctrine).
narrative_ontology:constraint_vindicates(marriage_authority_kernel__christian_canonical_reading, ecclesiastical_jurisdiction_over_marriage).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the dioceses and matrimonial tribunals through which Christian marriages are solemnized, registered, reviewed, and occasionally declared null. Define what counts as a valid marriage and what conduct warrants dissolution, and defend that definition before courts and legislatures. Their institutional standing rests on remaining the competent authority for their members' family life; stepping back from that role would unsettle the hierarchy's own self-understanding as custodian of the sacraments.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, church_hierarchies, agenda_setter,
    institutional, generational, identity_locked, national).

% Enacted and maintains the 1872 statute and the associated divorce legislation, and hears appeals from matrimonial causes. Gains an administrable settlement: family governance for the Christian community runs on a ready-made body of rules and institutions the state did not have to build. Bears the political cost whenever reform is attempted, since touching personal law mobilizes community opposition; retains the power to restructure the entire arrangement by legislation, as state-level uniform-code enactment demonstrates.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, indian_state_legislature_judiciary, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(marriage_authority_kernel__christian_canonical_reading, indian_state_legislature_judiciary, beneficiary).

% Staff the matrimonial tribunals: receive petitions, take testimony, issue recommendations on nullity and separation. Collect standing, office, and livelihood from the continuation of the tribunal system, whose caseload has thinned as civil courts absorb dissolution work. Their professional formation and position are bound to the tribunal apparatus; moving to purely pastoral roles would mean surrendering rank built inside it.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, clergy_tribunal_officials, beneficiary,
    organized, biographical, constrained, national).

% Uphold congregational expectations of marriage permanence, mediate family disputes informally, and apply social approval and sanction. A legal backdrop that treats marriage as hard to dissolve reinforces their teaching; they carry the day-to-day labor of holding families together under it. Their standing within the congregation depends on the norms they enforce remaining authoritative.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, conservative_community_elders, beneficiary,
    organized, generational, constrained, regional).

% Seek exit from broken or cruel marriages under grounds that historically recognized a husband's fault far more readily than a wife's, and that still require proving fault or obtaining the other spouse's consent. Individual leverage against church and court is slight; collective organization through women's fellowships, reform networks, and supported litigation is where their leverage actually lives. A civil marriage route exists but carries notice periods, procedural exposure, and loss of standing in the congregation.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, women_in_failed_marriages, payer,
    powerless, biographical, constrained, national).

% Live in dead marriages that match no statutory ground: no provable adultery, cruelty, or desertion, and no consenting partner. Before 2001 their remedy was nothing at all; since then mutual-consent divorce exists but fails whenever one spouse withholds consent, stranding the other without a path. Some possess the resources to relaunch life under the civil marriage act; many weigh that against community severance.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, spouses_without_fault_grounds, payer,
    moderate, biographical, constrained, national).

% Grow up inside marriages that neither spouse can dissolve and no tribunal will end, absorbing years of unresolved conflict. Parents speak for them, churches counsel around them, courts adjudicate over them; no proceeding in the arrangement takes their direct account of the household.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, children_of_stalled_marriages, excluded,
    powerless, biographical, trapped, local).

% Study the personal-law system comparatively and constitutionally, publish analyses of how the 1872 settlement distributes authority and burden, and testify in law-reform consultations. Positioned outside the dispute: they neither solemnize nor petition, and their assessments carry no vote in any tribunal.
narrative_ontology:constraint_stakeholder(marriage_authority_kernel__christian_canonical_reading, comparative_family_law_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(marriage_authority_kernel__christian_canonical_reading, church_hierarchies).
narrative_ontology:fixing_cost_class(marriage_authority_kernel__christian_canonical_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Determines, uniformly and with state backing, when a Christian marriage is validly formed, who may solemnize and register it, and how legitimacy, succession, and maintenance flow from it — solving recognition and succession-coordination problems for a geographically dispersed community served by many rites.
% TRANSFER_FUNCTION: Moves adjudicative authority over family breakdown from the couple to fault-proving before courts and tribunals; moves deference, institutional standing, and tribunal roles to the church hierarchy; historically moved dissolution access asymmetrically, granting husbands easier exit than wives.
% ABSENT_VOICES: Children of stalled marriages have no seat in any proceeding. Laywomen who never petition — the majority living under permanence teaching — reach the system only through intermediaries. Would-be civil marriers enter a conversation already structured between church authorities and the state, their objection pre-framed as defection from the community rather than as a competing claim about authority.
% DISAPPEARANCE_RATIONALE: If the arrangement vanished overnight, validity, registration, and succession rules would rebuild around the civil code within months; pending matrimonial causes would migrate to civil forums; the tribunals would lose their caseload and the clergy their offices; congregational discipline would lose its legal backdrop and fall back entirely on informal sanction. Marriages contracted under the old frame would need fresh recognition instruments. The world rearranges because every named seat's arrangements depend on the structure.
% FOUNDING_PROBLEM: Colonial administration needed a determinate answer to when a Christian marriage existed — to prevent bigamy, secure children's legitimacy and succession, and govern a population served by many missionary rites with divergent customs — and adopted the ready-made content of canon law, codifying it in 1872.
% FOUNDING_PROBLEM_CORROBORATION: Corroborated from outside the beneficiary set: colonial legislative council records attest the founding validity-and-bigamy problem; Law Commission of India consultation papers and parliamentary debate on the 2001 divorce amendments attest both that the recognition half remains live and that the dissolution regime persists as inherited discipline rather than answered need; independent legal-historical scholarship corroborates the genealogy. Church submissions attest the sacrament-protection rationale, but that attestation comes from inside the beneficiary set and is not counted as corroboration.
narrative_ontology:disappearance_verdict(marriage_authority_kernel__christian_canonical_reading, world_rearranges).
narrative_ontology:founding_problem_status(marriage_authority_kernel__christian_canonical_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(marriage_authority_kernel__christian_canonical_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_stealth2', 'agent/example_platform_commission.json',
    'stealth/ox-alpha', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(marriage_authority_kernel__christian_canonical_reading, 'none', 1).
narrative_ontology:epsilon_provenance(marriage_authority_kernel__christian_canonical_reading, 0.48, 'stealth/ox-alpha', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(marriage_authority_kernel__christian_canonical_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(marriage_authority_kernel__christian_canonical_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(marriage_authority_kernel__christian_canonical_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.48 at interval end) is moderate: the recognition, registration, and succession machinery delivers genuine coordination value, while the dissolution regime concentrates burden on those in failed marriages — historically with openly gendered grounds, formally equalized in 2001 but with practical residue flagged in an omega. Suppression (0.55) is authored as a raw structural property and is NOT scaled by power or scope — only extractiveness is scaled by the engine's directionality and scope computation. Suppression here mixes mechanisms: roughly structural (grounds requirements, procedural exposure of the civil-marriage route) and roughly internalized (formed belief that dissolution is sinful, congregational sanction), with the internalized share growing as legal barriers fell — handled by omega rather than by splitting the scalar. Theater ratio (0.41) rises across the interval because the tribunals' caseload thins as civil courts take dissolution: maintaining the tribunal apparatus increasingly performs continuity rather than adjudicates volume. Accessibility collapse (0.45) is well below mountain range because alternatives remain visible — the Special Marriage Act offers any citizen a civil route, and relocation or negotiated separation exist — but each alternative carries notice periods, procedural exposure, and loss of congregational standing. Resistance (0.55) reflects sustained reform movements, Law Commission critique, women's petitions, and uniform-code advocacy meeting institutional defense of the reading. The temporal series run on ONE shared grid (t = 0, 25, 50, 75, 100, 125, 154; one unit approximates one year since 1872, so t=75 is independence, t=125 precedes the 2001 amendments, t=154 is the present): extraction and suppression decline as amendments and alternatives accumulated, theater rises as function migrated to civil forums. Enforcement-capacity change is traced deliberately, hence the suppression_requirement series; the picture is monotonic decay of coercive intensity, not oscillation, so no cyclical analysis is required.
 *
 * PERSPECTIVAL GAP:
 *   The seats should compute differently. From the church_hierarchies seat the arrangement is a trust it administers: valid marriage, protected family, orderly nullity — a coordination frame it experiences as stewardship, with the burden side invisible from that chair. From the payer seats the same structure operates as a locked door: proof obligations, withheld consents, and congregational cost attached to every exit. The state seat sees an administrable settlement it did not have to design, plus an electoral liability it cannot cheaply touch. Identity-lock dynamics bind the church seat specifically: this is INSTITUTIONAL identity fusion — the hierarchy has become its jurisdiction over family life, so exiting the role would not relocate the institution but dissolve its self-concept as custodian of the sacraments; if that frame broke, the church seat would recompute from steward to administrator of a delegable function, and the arrangement's enforcement would lose its most committed defender. Coalition dynamics cut the other way for the powerless payer seat: individually slight, women in failed marriages aggregate real leverage through fellowships, reform networks, and litigation support — the engine should read their effective power as coalition-mediated, not atomistic. The scholar seat, holding analytical exit, computes the whole structure without bearing any of it.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiary declarations drive low directionality: church_hierarchies (collects authority, deference, and the tribunal system's continuation), clergy_tribunal_officials (collect standing and roles), and conservative_community_elders (collect a legal backdrop for congregational teaching) sit near the subsidized end. Victim declarations drive high directionality: women_in_failed_marriages and spouses_without_fault_grounds bear the dissolution regime's concentrated costs with constrained exits, sitting near the full-target end — the women's seat nearer it, given the historical asymmetry of grounds and thinner individual resources. The state is genuinely dual-positioned: it administers and collects administrative convenience (pulling d down) while absorbing the political cost of every reform attempt (pushing d up), netting to a mildly beneficiary position rather than the near-pure-beneficiary read its secondary collection alone would suggest. Children of stalled marriages hold an excluded seat whose interests register nowhere in the derivation — recorded as absence, not as a directional correction. Comparative scholars sit at the analytical pole: no collection, no payment.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem splits. Its validity-and-recognition half (when does a Christian marriage exist; who may solemnize; how do legitimacy and succession flow) remains live — the machinery still answers real questions daily. Its discipline half (holding marriages permanently open to scrutiny, restricting dissolution to grave fault) is contested: the 2001 mutual-consent route concedes that the problem it answered no longer commands assent even within the arrangement's own statute book. Mandatrophy is therefore NOT declared resolved: the mandate has partially outlived one of its two functions, but the surviving function is load-bearing, so the arrangement is not a shell. The tangled_rope classification is what prevents both mislabels here: reading the structure as pure extraction erases the recognition and succession coordination that payers themselves still use and want; reading it as pure coordination erases the gendered, enforced burden that made reform movements necessary. The rising theater series is the early-warning line: if tribunal maintenance becomes fully performative while civil forums absorb all function, the residual structure drifts toward inertial persistence — the omega on uniform-code displacement tracks exactly that transition.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'This constraint is one reading (christian_canonical_reading) of the marriage_authority_kernel; what would each sibling reading change structurally if instantiated instead?',
    'Generate the four sibling stories (hindu_codified, muslim_shariat, parsi_communal, secular_civil) and compare victim sets, epsilon, and computed types across the family.',
    'The secular_civil sibling would relocate the victim set to everyone denied a purely civil exit and raise measured extraction; the communal siblings would shift the enforcement locus to different institutions while keeping a similar coordination-and-burden mix.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'Committer-frame position: one reading of a contested kernel; siblings are other constraints, not errors in this one.').

omega_variable(
    authority_source_disagreement_location,
    'Where exactly do the readings of the kernel disagree, such that resolution would bite?',
    'Locate the disputed element: the SOURCE of binding authority (divine/canonical warrant versus codified custom versus revealed law versus constitutional individual right), not the content of particular divorce rules.',
    'If the disagreement sits in the authority source, no adjustment of divorce grounds reconciles the readings; classification divergence across the family is structural rather than parametric.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(authority_source_disagreement_location, conceptual, 'Disagreement location for the kernel contest.').

omega_variable(
    gender_asymmetry_residue,
    'Does material gender asymmetry persist in practice after the 2001 amendments formally equalized divorce grounds?',
    'Court statistics on petition outcomes by petitioner gender; matrimonial tribunal records; qualitative studies of women''s exit experiences after the amendment.',
    'Persistent asymmetry keeps the payer concentration gendered and supports the higher end of measured extraction; demonstrated parity would move the arrangement toward symmetric coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gender_asymmetry_residue, empirical, 'Formal versus practical gender parity in dissolution access.').

omega_variable(
    suppression_structural_vs_internalized,
    'Is the measured suppression structural (statutory grounds, procedural barriers) or internalized (formed belief that dissolution is sinful, fear of congregational sanction persisting after legal barriers were lowered)?',
    'Compare post-2001 petition uptake against estimated latent demand: if demand for dissolution stayed low after legal routes opened, a large internalized share is indicated.',
    'Internalized suppression travels with the agent after legal exit opens, keeping effective suppression above the structural measure; the working estimate attributes roughly 60 percent to structural and 40 percent to internalized mechanisms at interval end, shifting toward internalized over time.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_structural_vs_internalized, empirical, 'Split of suppression between legal barrier and formed conscience or congregational pressure.').

omega_variable(
    ucc_displacement_trajectory,
    'Will uniform-civil-code adoption (Uttarakhand 2024 as precedent) displace this reading''s jurisdiction wholesale, region by region?',
    'Track state-level uniform-code enactments and litigation over their application to Christians; watch whether a national code is enacted and whether personal-law carve-outs survive.',
    'Wholesale displacement converts the arrangement into a transitional remnant whose remaining activity is ceremonial maintenance; partial displacement leaves the mixed structure intact with raised resistance and sharper seat divergence.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ucc_displacement_trajectory, empirical, 'Whether the reading''s jurisdiction is being displaced by civil-code expansion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(marriage_authority_kernel__christian_canonical_reading, 0, 154).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mak_xian_canon_tr_t0, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(mak_xian_canon_tr_t0, observed).
narrative_ontology:measurement(mak_xian_canon_tr_t25, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 25, 0.22).
narrative_ontology:measurement_basis(mak_xian_canon_tr_t25, observed).
narrative_ontology:measurement(mak_xian_canon_tr_t50, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 50, 0.27).
narrative_ontology:measurement_basis(mak_xian_canon_tr_t50, observed).
narrative_ontology:measurement(mak_xian_canon_tr_t75, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 75, 0.31).
narrative_ontology:measurement_basis(mak_xian_canon_tr_t75, observed).
narrative_ontology:measurement(mak_xian_canon_tr_t100, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 100, 0.34).
narrative_ontology:measurement_basis(mak_xian_canon_tr_t100, observed).
narrative_ontology:measurement(mak_xian_canon_tr_t125, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 125, 0.38).
narrative_ontology:measurement_basis(mak_xian_canon_tr_t125, observed).
narrative_ontology:measurement(mak_xian_canon_tr_t154, marriage_authority_kernel__christian_canonical_reading, theater_ratio, 154, 0.41).
narrative_ontology:measurement_basis(mak_xian_canon_tr_t154, observed).

% Extraction over time
narrative_ontology:measurement(mak_xian_canon_be_t0, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 0, 0.62).
narrative_ontology:measurement_basis(mak_xian_canon_be_t0, observed).
narrative_ontology:measurement(mak_xian_canon_be_t25, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 25, 0.6).
narrative_ontology:measurement_basis(mak_xian_canon_be_t25, observed).
narrative_ontology:measurement(mak_xian_canon_be_t50, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 50, 0.58).
narrative_ontology:measurement_basis(mak_xian_canon_be_t50, observed).
narrative_ontology:measurement(mak_xian_canon_be_t75, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 75, 0.56).
narrative_ontology:measurement_basis(mak_xian_canon_be_t75, observed).
narrative_ontology:measurement(mak_xian_canon_be_t100, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 100, 0.52).
narrative_ontology:measurement_basis(mak_xian_canon_be_t100, observed).
narrative_ontology:measurement(mak_xian_canon_be_t125, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 125, 0.49).
narrative_ontology:measurement_basis(mak_xian_canon_be_t125, observed).
narrative_ontology:measurement(mak_xian_canon_be_t154, marriage_authority_kernel__christian_canonical_reading, base_extractiveness, 154, 0.48).
narrative_ontology:measurement_basis(mak_xian_canon_be_t154, observed).

% Suppression requirement over time
narrative_ontology:measurement(mak_xian_canon_su_t0, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 0, 0.74).
narrative_ontology:measurement_basis(mak_xian_canon_su_t0, observed).
narrative_ontology:measurement(mak_xian_canon_su_t25, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(mak_xian_canon_su_t25, observed).
narrative_ontology:measurement(mak_xian_canon_su_t50, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 50, 0.69).
narrative_ontology:measurement_basis(mak_xian_canon_su_t50, observed).
narrative_ontology:measurement(mak_xian_canon_su_t75, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 75, 0.65).
narrative_ontology:measurement_basis(mak_xian_canon_su_t75, observed).
narrative_ontology:measurement(mak_xian_canon_su_t100, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 100, 0.61).
narrative_ontology:measurement_basis(mak_xian_canon_su_t100, observed).
narrative_ontology:measurement(mak_xian_canon_su_t125, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 125, 0.57).
narrative_ontology:measurement_basis(mak_xian_canon_su_t125, observed).
narrative_ontology:measurement(mak_xian_canon_su_t154, marriage_authority_kernel__christian_canonical_reading, suppression_requirement, 154, 0.55).
narrative_ontology:measurement_basis(mak_xian_canon_su_t154, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(marriage_authority_kernel__christian_canonical_reading, resource_allocation).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__hindu_codified_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__muslim_shariat_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__parsi_communal_reading).
narrative_ontology:affects_constraint(marriage_authority_kernel__christian_canonical_reading, marriage_authority_kernel__secular_civil_reading).

% DUAL FORMULATION NOTE:
% Constraint-family decomposition per the epsilon-invariance principle: the colloquial label 'religious personal law in India' is one kernel (the source of marriage/family law authority) instantiated as FIVE structurally distinct constraints, one per reading. Each member has its own authority source, victim set, and epsilon; measuring them through the single label would average away exactly the differences the corpus exists to take. Edge semantics from this reading: the three communal siblings COEXIST with it (different communities, simultaneous operation, no party's framework eliminates another), and this reading INFLUENCES the secular_civil sibling — its century-and-a-half entrenchment is the principal structural obstacle any uniform code must displace or accommodate, and conversely its stability is routinely cited by the other communal readings as precedent for personal-law entitlement, so erosion here propagates legitimacy pressure across the whole family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
